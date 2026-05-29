#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# <xbar.title>Stock Holder</xbar.title>
# <xbar.version>3.0</xbar.version>
# <xbar.desc>股票持仓监控，实时展示盈亏金额及持仓详情，支持初始化持仓、加仓、减仓、持久化。</xbar.desc>
# <xbar.dependencies>python3</xbar.dependencies>

import sys
import os
import json
import urllib.request
import subprocess
import threading
from http.server import HTTPServer, BaseHTTPRequestHandler
from datetime import datetime, date

# ── 路径 ──────────────────────────────────────────────────────────────────────
DATA_DIR     = os.path.join(os.path.dirname(os.path.abspath(__file__)), ".config")
STOCK_INFO_F = os.path.join(DATA_DIR, "stock_info.json")

os.makedirs(DATA_DIR, exist_ok=True)

# ── 常量 ──────────────────────────────────────────────────────────────────────
LOT_SIZE_NORMAL = 100
LOT_SIZE_STAR   = 200   # 科创板 688 开头

def lot_size(code):
    return LOT_SIZE_STAR if str(code).startswith("688") else LOT_SIZE_NORMAL

def build_code(s):
    return ("sh" if s.startswith("6") else "sz") + s

def fmt(val):
    """带符号两位小数"""
    sign = "+" if val >= 0 else ""
    return f"{sign}{val:.2f}"

# ── JSON 读写 ──────────────────────────────────────────────────────────────────
def load_info():
    if os.path.exists(STOCK_INFO_F):
        try:
            with open(STOCK_INFO_F, "r", encoding="utf-8") as f:
                return json.load(f)
        except Exception:
            pass
    return {"holdings": [], "quote": []}

def save_info(info):
    with open(STOCK_INFO_F, "w", encoding="utf-8") as f:
        json.dump(info, f, ensure_ascii=False, indent=2)

def get_holdings(info):
    return {h["code"]: h for h in info.get("holdings", [])}

def get_quotes(info):
    return {q["code"]: q for q in info.get("quote", [])}

# ── 行情接口 ──────────────────────────────────────────────────────────────────
def fetch_quotes(codes_6digit):
    """返回 dict: code6 -> {name, price, prev_close, change, pct}"""
    if not codes_6digit:
        return {}
    full = [build_code(c) for c in codes_6digit]
    url  = "https://qt.gtimg.cn/q=" + ",".join(full)
    req  = urllib.request.Request(url, headers={"Referer": "https://finance.qq.com"})
    raw  = urllib.request.urlopen(req, timeout=10).read().decode("gbk", errors="replace")
    result = {}
    for line in raw.strip().splitlines():
        if '"' not in line:
            continue
        fields = line.split('"')[1].split("~")
        code6  = fields[2]
        result[code6] = {
            "name":       fields[1],
            "price":      float(fields[3]),
            "prev_close": float(fields[4]),
            "change":     float(fields[31]) if fields[31] else 0.0,
            "pct":        float(fields[32]) if fields[32] else 0.0,
        }
    return result

def fetch_name(code6):
    q = fetch_quotes([code6])
    return q.get(code6, {}).get("name", code6)

# ── 时段判断 ──────────────────────────────────────────────────────────────────
def trading_status():
    """weekend | open | closing_noon | closing | after_close | closed"""
    now = datetime.now()
    if now.weekday() >= 5:   # 5=周六 6=周日
        return "weekend"
    t = now.time()
    def tm(s): return datetime.strptime(s, "%H:%M").time()
    if (tm("09:30") <= t < tm("11:30")) or (tm("13:00") <= t < tm("15:00")):
        return "open"
    if tm("11:30") <= t < tm("13:00"):
        return "closing_noon"  # 11:30–12:59，首次触发写入，后续读缓存
    if tm("15:00") <= t < tm("16:00"):
        return "closing"       # 15:00–15:59，首次触发写入，后续读缓存
    if t >= tm("16:00"):
        return "after_close"   # 16:00+ 纯展示缓存
    return "closed"

# ── holdings 操作 ──────────────────────────────────────────────────────────────
def apply_init(info, entries):
    """初始化：请求接口拿名称+行情，写入 holdings 和 quote"""
    codes = [e["code"] for e in entries]
    try:
        quotes = fetch_quotes(codes)
    except Exception:
        quotes = {}

    hmap    = get_holdings(info)
    qmap    = get_quotes(info)
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M")

    for e in entries:
        code = e["code"]
        lots = float(e["lots"])
        cost = round(float(e["cost"]), 4)
        q    = quotes.get(code, {})
        name = q.get("name") or code

        hmap[code] = {
            "code":           code,
            "name":           name,
            "cost":           cost,
            "lots":           lots,
            "available_lots": lots,
        }

        # 写入 quote（当前行情快照）
        price = q.get("price")
        if price is not None:
            shares    = lots * lot_size(code)
            prev      = q.get("prev_close") or price
            change    = q.get("change", 0.0)
            day_pnl   = change * shares
            day_pct   = (change / prev * 100) if prev else 0
            total_pnl = (price - cost) * shares
            total_pct = ((price - cost) / cost * 100) if cost else 0
            qmap[code] = {
                "code":       code,
                "price":      round(price, 4),
                "market_val": round(price * shares, 2),
                "day_pnl":    round(day_pnl, 2),
                "day_pct":    round(day_pct, 2),
                "total_pnl":  round(total_pnl, 2),
                "total_pct":  round(total_pct, 2),
                "date":       now_str,
            }

    info["holdings"] = list(hmap.values())
    info["quote"]    = list(qmap.values())
    save_info(info)

# ── operate 操作盈亏（按天隔离） ────────────────────────────────────────────────
def _clean_operate(info):
    """清除非今日的 operate 记录（每次读取时调用）"""
    today = date.today().isoformat()
    ops = info.get("operate", [])
    info["operate"] = [o for o in ops if o.get("date") == today]

def _record_operate(info, code, pnl, open_lots, today):
    """在 operate 中累加当日该 code 的操作盈亏；open_lots 仅首次写入（代表开盘持仓手数）"""
    ops = info.setdefault("operate", [])
    for o in ops:
        if o["code"] == code and o["date"] == today:
            o["pnl"] = round(o["pnl"] + pnl, 2)
            return
    ops.append({"code": code, "pnl": round(pnl, 2), "open_lots": open_lots, "date": today})

def get_today_operate(info):
    """返回 {code: {pnl, open_lots}} 当日操作映射"""
    today = date.today().isoformat()
    return {o["code"]: o for o in info.get("operate", []) if o.get("date") == today}

def apply_buy(info, entries):
    """加仓：更新 holdings（cost/lots），记录当日操作盈亏到 operate"""
    hmap  = get_holdings(info)
    today = date.today().isoformat()

    all_codes = [e["code"] for e in entries]
    try:
        live = fetch_quotes(all_codes)
    except Exception:
        live = {}

    for e in entries:
        code     = e["code"]
        price    = float(e["price"])
        buy_lots = float(e["lots"])
        q        = live.get(code, {})
        prev_close = q.get("prev_close") or q.get("price") or price

        if code in hmap:
            old      = hmap[code]
            new_lots = old["lots"] + buy_lots
            new_cost = (old["cost"] * old["lots"] + price * buy_lots) / new_lots
            hmap[code]["cost"] = round(new_cost, 4)
            hmap[code]["lots"] = new_lots
        else:
            name = q.get("name") or code
            hmap[code] = {
                "code":           code,
                "name":           name,
                "cost":           round(price, 4),
                "lots":           buy_lots,
                "available_lots": 0,
            }

        # 加仓操作盈亏：(0轴 - 买入价) * 手数 * 每手股数
        # open_lots：本次操作前的持仓手数（首次加仓该code当天时记录，作为day_pct分母基础）
        open_lots = hmap[code]["lots"] - buy_lots  # 加仓后才更新了lots，减回去还原操作前
        op_pnl = (prev_close - price) * buy_lots * lot_size(code)
        _record_operate(info, code, op_pnl, open_lots, today)

    info["holdings"] = list(hmap.values())
    save_info(info)

def apply_sell(info, entries):
    """entries: [{code, price, lots}]  返回收益通知列表"""
    hmap  = get_holdings(info)
    qmap  = get_quotes(info)
    today = date.today().isoformat()
    msgs  = []
    errors = []

    all_codes = [e["code"] for e in entries if e["code"] in hmap]
    try:
        live = fetch_quotes(all_codes) if all_codes else {}
    except Exception:
        live = {}

    for e in entries:
        code      = e["code"]
        price     = float(e["price"])
        sell_lots = float(e["lots"])
        if code not in hmap:
            errors.append(f"{code} 无持仓记录")
            continue
        h = hmap[code]
        if sell_lots > h["available_lots"]:
            errors.append(f"{code} 可用手数不足（可用 {h['available_lots']}，卖出 {sell_lots}）")
            continue

        q          = live.get(code, {})
        prev_close = q.get("prev_close") or q.get("price") or h["cost"]

        # 减仓操作盈亏：(卖出价 - 0轴) * 手数 * 每手股数
        # open_lots：操作前持仓（此时 h["lots"] 还未扣减）
        open_lots = h["lots"]
        op_pnl = (price - prev_close) * sell_lots * lot_size(code)
        _record_operate(info, code, op_pnl, open_lots, today)

        # 更新持仓（成本不变，只减手数）
        h["lots"]           -= sell_lots
        h["available_lots"] -= sell_lots

        msgs.append(f"{h['name']} 卖出 {sell_lots} 手，操作收益 {fmt(op_pnl)}")

        # 减仓至 0 时保留持仓记录（lots=0），用于当日继续展示收益
        # 只有收盘结算后才清除，不在此处删除

    info["holdings"] = list(hmap.values())
    info["quote"]    = list(qmap.values())
    save_info(info)
    return msgs, errors

# ── 收盘写入 / 次日清空 ───────────────────────────────────────────────────────
def write_closing_quote(info, quotes, session="afternoon"):
    """收盘时把最终行情写入 quote；session='noon' 为中午快照，'afternoon' 为收盘定稿"""
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M")
    hmap = get_holdings(info)
    rows = []
    for code, h in hmap.items():
        q = quotes.get(code)
        if not q:
            continue
        shares    = h["lots"] * lot_size(code)
        cost      = h["cost"]
        price     = q["price"]
        prev      = q.get("prev_close") or price
        change    = q.get("change", 0.0)
        day_pnl   = change * shares
        day_pct   = (change / prev * 100) if prev else 0
        total_pnl = (price - cost) * shares
        total_pct = ((price - cost) / cost * 100) if cost else 0
        rows.append({
            "code":       code,
            "price":      round(price, 4),
            "market_val": round(price * shares, 2),
            "day_pnl":    round(day_pnl, 2),
            "day_pct":    round(day_pct, 2),
            "total_pnl":  round(total_pnl, 2),
            "total_pct":  round(total_pct, 2),
            "date":       now_str,
        })
    info["quote"] = rows
    # 收盘结算：清除当天减仓至 0 的持仓记录（仅下午收盘时清理）
    if session == "afternoon":
        info["holdings"] = [h for h in info.get("holdings", []) if h.get("lots", 0) > 0]
    save_info(info)

def reset_for_new_day(info):
    pass  # 不再需要，保留占位避免引用报错

# ── HTML 表单弹窗 ──────────────────────────────────────────────────────────────
HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="zh">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
<style>
  * {{ box-sizing: border-box; margin: 0; padding: 0; }}
  html, body {{
    font-family: -apple-system, BlinkMacSystemFont, "Helvetica Neue", sans-serif;
    background: #f5f5f7;
  }}
  body {{
    display: flex;
    justify-content: center;
    align-items: flex-start;
    padding: 24px 16px 40px;
  }}
  .card {{
    background: #fff;
    border-radius: 12px;
    padding: 22px 20px 18px;
    width: 100%;
    max-width: 560px;
    box-shadow: 0 2px 14px rgba(0,0,0,.08);
  }}
  h2 {{ font-size: 16px; font-weight: 600; margin-bottom: 4px; color: #1d1d1f; }}
  .hint {{ font-size: 12px; color: #8e8e93; margin-bottom: 14px; }}

  /* 列标题 */
  .col-header {{
    display: grid;
    grid-template-columns: {col_tpl} 28px;
    gap: 6px;
    margin-bottom: 4px;
    padding: 0 2px;
  }}
  .col-header span {{ font-size: 11px; color: #8e8e93; font-weight: 500; }}

  /* 每行 */
  .row {{
    display: grid;
    grid-template-columns: {col_tpl} 28px;
    gap: 6px;
    margin-bottom: 10px;
    align-items: start;
  }}

  /* 通用输入样式 */
  .field {{ display: flex; flex-direction: column; gap: 3px; min-width: 0; }}
  .field input, .field select {{
    width: 100%;
    min-width: 0;
    padding: 6px 8px;
    border: 1px solid #d2d2d7;
    border-radius: 7px;
    font-size: 13px;
    outline: none;
    background: #fff;
    transition: border-color .15s;
    -webkit-appearance: none;
  }}
  .field input:focus, .field select:focus {{ border-color: #0071e3; }}
  .field input.err, .field select.err {{ border-color: #e00; }}
  .field .sub {{ font-size: 10px; color: #aeaeb2; padding: 0 2px; line-height: 1.4; }}

  /* 自定义 select 箭头 */
  .select-wrap {{ position: relative; }}
  .select-wrap::after {{
    content: "▾";
    position: absolute;
    right: 8px; top: 50%; transform: translateY(-50%);
    font-size: 11px; color: #8e8e93; pointer-events: none;
  }}
  .select-wrap select {{ padding-right: 22px; }}

  /* 加仓代码：下拉 + 手动输入切换 */
  .code-wrap {{ display: flex; flex-direction: column; gap: 3px; }}
  .code-toggle {{
    font-size: 10px; color: #0071e3; cursor: pointer;
    text-align: right; padding-right: 2px;
    user-select: none;
  }}
  .code-toggle:hover {{ text-decoration: underline; }}

  .del {{
    width: 28px; height: 28px;
    background: none; border: none; cursor: pointer;
    color: #c7c7cc; font-size: 18px; line-height: 28px;
    text-align: center; border-radius: 5px;
    margin-top: 2px;
  }}
  .del:hover {{ color: #e00; background: #fff0f0; }}

  .add-btn {{
    margin-top: 2px; background: none;
    border: 1.5px dashed #c7c7cc; border-radius: 7px;
    width: 100%; padding: 6px; font-size: 13px;
    color: #8e8e93; cursor: pointer;
    transition: border-color .15s, color .15s;
  }}
  .add-btn:hover {{ border-color: #0071e3; color: #0071e3; }}

  #msg {{ font-size: 12px; color: #e00; margin-top: 8px; min-height: 16px; }}
  .footer {{ display: flex; justify-content: flex-end; gap: 8px; margin-top: 16px; }}
  .btn {{
    padding: 7px 18px; border-radius: 7px; font-size: 14px;
    border: none; cursor: pointer; font-weight: 500;
  }}
  .cancel {{ background: #e8e8ed; color: #1d1d1f; }}
  .submit {{ background: #0071e3; color: #fff; }}
  .submit:hover {{ background: #005bbf; }}
</style>
</head>
<body>
<div class="card">
  <h2>{title}</h2>
  <p class="hint">{hint}</p>
  <div class="col-header" id="col-header"></div>
  <div id="rows"></div>
  <button class="add-btn" onclick="addRow()">＋ 添加一条</button>
  <div id="msg"></div>
  <div class="footer">
    <button class="btn cancel" onclick="doCancel()">取消</button>
    <button class="btn submit" onclick="doSubmit()">确定</button>
  </div>
</div>
<script>
const FIELDS    = {fields_json};
const HOLDINGS  = {holdings_json};  // [{{code, name}}]
const CMD       = "{cmd}";
let rowCount = 0;

// 列标题
const header = document.getElementById('col-header');
FIELDS.forEach(f => {{
  const s = document.createElement('span');
  s.textContent = f.label;
  header.appendChild(s);
}});
header.insertAdjacentHTML('beforeend', '<span></span>');

function makeCodeField(f) {{
  // sell: 纯下拉，只能选持仓
  // buy:  下拉优先，可切换为手动输入
  // init: 纯文本输入
  const wrap = document.createElement('div');
  wrap.className = 'field';

  if (CMD === 'sell') {{
    const sw = document.createElement('div');
    sw.className = 'select-wrap';
    const sel = document.createElement('select');
    sel.name = 'code';
    const blank = document.createElement('option');
    blank.value = ''; blank.textContent = '选择持仓股票';
    sel.appendChild(blank);
    HOLDINGS.forEach(h => {{
      const opt = document.createElement('option');
      opt.value = h.code;
      opt.textContent = h.name + '（' + h.code + '）';
      sel.appendChild(opt);
    }});
    sw.appendChild(sel);
    wrap.appendChild(sw);
  }} else if (CMD === 'buy') {{
    // 下拉模式（默认）
    const sw = document.createElement('div');
    sw.className = 'select-wrap';
    sw.id = wrap.id + '-sw';
    const sel = document.createElement('select');
    sel.name = 'code';
    const blank = document.createElement('option');
    blank.value = ''; blank.textContent = '选择已有持仓';
    sel.appendChild(blank);
    HOLDINGS.forEach(h => {{
      const opt = document.createElement('option');
      opt.value = h.code;
      opt.textContent = h.name + '（' + h.code + '）';
      sel.appendChild(opt);
    }});
    sw.appendChild(sel);
    wrap.appendChild(sw);

    // 手动输入模式（隐藏）
    const inp = document.createElement('input');
    inp.name = 'code'; inp.placeholder = '输入6位股票代码';
    inp.style.display = 'none';
    wrap.appendChild(inp);

    // 切换按钮
    const toggle = document.createElement('span');
    toggle.className = 'code-toggle';
    toggle.textContent = '手动输入代码 ›';
    let manual = false;
    toggle.onclick = () => {{
      manual = !manual;
      sw.style.display  = manual ? 'none' : 'block';
      inp.style.display = manual ? 'block' : 'none';
      sel.name = manual ? '' : 'code';
      inp.name = manual ? 'code' : '';
      toggle.textContent = manual ? '‹ 从持仓选择' : '手动输入代码 ›';
    }};
    wrap.appendChild(toggle);
  }} else {{
    // init: 纯文本
    const inp = document.createElement('input');
    inp.name = 'code'; inp.placeholder = f.placeholder || '6位股票代码';
    inp.autocomplete = 'off';
    wrap.appendChild(inp);
  }}

  if (f.hint) {{
    const sub = document.createElement('span');
    sub.className = 'sub';
    sub.textContent = f.hint;
    wrap.appendChild(sub);
  }}
  return wrap;
}}

function makeField(f) {{
  if (f.key === 'code') return makeCodeField(f);
  const wrap = document.createElement('div');
  wrap.className = 'field';
  const inp = document.createElement('input');
  inp.name = f.key;
  inp.placeholder = f.placeholder || f.label;
  inp.autocomplete = 'off';
  if (f.inputmode) inp.inputMode = f.inputmode;
  wrap.appendChild(inp);
  if (f.hint) {{
    const sub = document.createElement('span');
    sub.className = 'sub';
    sub.textContent = f.hint;
    wrap.appendChild(sub);
  }}
  return wrap;
}}

function addRow() {{
  const container = document.getElementById('rows');
  const div = document.createElement('div');
  div.className = 'row';
  div.id = 'row-' + rowCount;
  FIELDS.forEach(f => div.appendChild(makeField(f)));
  const del = document.createElement('button');
  del.className = 'del'; del.title = '删除'; del.textContent = '×';
  del.onclick = () => {{ div.remove(); updateDelButtons(); }};
  div.appendChild(del);
  container.appendChild(div);
  rowCount++;
  updateDelButtons();
  // 聚焦第一个可见输入/select
  const first = div.querySelector('input:not([style*="display: none"]), select');
  if (first) first.focus();
}}

function updateDelButtons() {{
  const rows = document.querySelectorAll('#rows .row');
  rows.forEach(r => {{
    const btn = r.querySelector('.del');
    if (btn) btn.disabled = rows.length === 1;
  }});
}}

function getRowValues(row) {{
  const entry = {{}};
  // 收集所有有 name 且可见的 input/select
  row.querySelectorAll('input[name], select[name]').forEach(el => {{
    if (el.name && el.style.display !== 'none') {{
      entry[el.name] = el.value.trim();
    }}
  }});
  return entry;
}}

function doCancel() {{
  fetch('/cancel').finally(() => window.close());
}}

function doSubmit() {{
  const rows = document.querySelectorAll('#rows .row');
  const data = []; let ok = true;
  rows.forEach(row => {{
    const entry = getRowValues(row);
    let rowOk = true;
    // 校验所有字段非空
    FIELDS.forEach(f => {{
      const val = entry[f.key];
      const el = row.querySelector(`[name="${{f.key}}"]`);
      if (!val) {{ rowOk = false; ok = false; if (el) el.classList.add('err'); }}
      else {{ if (el) el.classList.remove('err'); }}
    }});
    if (rowOk) data.push(entry);
  }});
  if (!ok) {{ document.getElementById('msg').textContent = '请填写所有字段'; return; }}
  document.getElementById('msg').textContent = '';
  fetch('/submit', {{
    method: 'POST',
    headers: {{'Content-Type': 'application/json'}},
    body: JSON.stringify(data)
  }})
  .then(r => r.json())
  .then(res => {{
    if (res.ok) window.close();
    else document.getElementById('msg').textContent = res.error || '操作失败';
  }});
}}

document.addEventListener('keydown', e => {{
  if (e.key !== 'Enter') return;
  const all = [...document.querySelectorAll('#rows input:not([style*="display: none"])')];
  const idx = all.indexOf(document.activeElement);
  if (idx >= 0 && idx < all.length - 1) {{ all[idx+1].focus(); e.preventDefault(); }}
  else if (idx === all.length - 1) {{ doSubmit(); e.preventDefault(); }}
}});

addRow();
</script>
</body>
</html>
"""

FORM_CONFIGS = {
    "init": {
        "title": "初始化持仓",
        "hint":  "填写股票代码、成本均价和持仓手数，可添加多条",
        "fields": [
            {"key": "code", "label": "股票代码",
             "placeholder": "如 603601", "hint": "6位沪深股票代码"},
            {"key": "cost", "label": "成本均价",
             "placeholder": "如 17.23", "hint": "建仓均价（元/股）", "inputmode": "decimal"},
            {"key": "lots", "label": "持仓手数",
             "placeholder": "如 2", "hint": "1手=100股，科创板200股", "inputmode": "decimal"},
        ],
    },
    "buy": {
        "title": "加仓",
        "hint":  "选择已有持仓或手动输入代码，填写买入价和手数",
        "fields": [
            {"key": "code", "label": "股票",
             "hint": "可从持仓选择或手动输入新代码"},
            {"key": "price", "label": "买入价",
             "placeholder": "如 17.50", "hint": "本次买入价格（元/股）", "inputmode": "decimal"},
            {"key": "lots", "label": "买入手数",
             "placeholder": "如 1", "hint": "1手=100股，科创板200股", "inputmode": "decimal"},
        ],
    },
    "sell": {
        "title": "减仓",
        "hint":  "从持仓中选择股票，填写卖出价和手数",
        "fields": [
            {"key": "code", "label": "持仓股票",
             "hint": "只能从当前持仓中选择"},
            {"key": "price", "label": "卖出价",
             "placeholder": "如 18.00", "hint": "本次卖出价格（元/股）", "inputmode": "decimal"},
            {"key": "lots",  "label": "卖出手数",
             "placeholder": "如 1", "hint": "不超过可用手数", "inputmode": "decimal"},
        ],
    },
}

def run_form(cmd):
    """启动本地 HTTP server，弹出浏览器表单，阻塞直到用户提交或取消"""
    cfg    = FORM_CONFIGS[cmd]
    result = {"done": False}
    info   = load_info()

    # 持仓列表注入页面（code + name）
    holdings_list = [{"code": h["code"], "name": h["name"]}
                     for h in info.get("holdings", [])]

    # grid 列宽：code 列稍宽，其余等分
    col_tpl = "1.4fr 1fr 1fr"

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *a): pass

        def do_GET(self):
            if self.path == "/":
                html = HTML_TEMPLATE.format(
                    title        = cfg["title"],
                    hint         = cfg["hint"],
                    fields_json  = json.dumps(cfg["fields"], ensure_ascii=False),
                    holdings_json= json.dumps(holdings_list, ensure_ascii=False),
                    cmd          = cmd,
                    col_tpl      = col_tpl,
                )
                self._respond(200, "text/html", html.encode())
            elif self.path == "/cancel":
                result["done"] = True
                self._respond(200, "application/json", b'{"ok":true}')
            else:
                self._respond(404, "text/plain", b"not found")

        def do_POST(self):
            if self.path == "/submit":
                length  = int(self.headers.get("Content-Length", 0))
                body    = self.rfile.read(length)
                entries = json.loads(body)
                # 校验 + 执行
                err = None
                try:
                    if cmd == "init":
                        apply_init(info, entries)
                        notify(cfg["title"] + f" {len(entries)} 条已保存")
                    elif cmd == "buy":
                        apply_buy(info, entries)
                        notify(cfg["title"] + f" {len(entries)} 条已保存")
                    elif cmd == "sell":
                        msgs, errors = apply_sell(info, entries)
                        if errors:
                            err = "；".join(errors)
                        else:
                            notify("\n".join(msgs))
                except Exception as ex:
                    err = str(ex)

                if err:
                    resp = json.dumps({"ok": False, "error": err}).encode()
                else:
                    result["done"] = True
                    resp = json.dumps({"ok": True}).encode()
                self._respond(200, "application/json", resp)
            else:
                self._respond(404, "text/plain", b"not found")

        def _respond(self, code, ct, body):
            self.send_response(code)
            self.send_header("Content-Type", ct + "; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

    server = HTTPServer(("127.0.0.1", 0), Handler)
    port   = server.server_address[1]

    # 后台处理请求
    def serve():
        while not result["done"]:
            server.handle_request()
        server.server_close()

    t = threading.Thread(target=serve, daemon=True)
    t.start()

    subprocess.Popen(["open", f"http://127.0.0.1:{port}/"])
    t.join(timeout=120)   # 最多等 2 分钟

# ── 系统通知 ──────────────────────────────────────────────────────────────────
def notify(msg):
    safe = msg.replace('"', '\\"')
    subprocess.run(
        ["osascript", "-e", f'display notification "{safe}" with title "股票助手"'],
        capture_output=True,
    )

# ── 菜单输出 ──────────────────────────────────────────────────────────────────
FONT = "font=Menlo size=12"

# 各列视觉宽度（英文字符数）：中文字符算2宽
# col1:股票名(5中文=10) col2:今日盈亏 col3:成本/现价(涨幅) col4:总盈亏 col5:持仓/可用
COL_WIDTHS = [16, 18, 20, 18, 8]

def visual_len(s):
    """计算字符串的视觉宽度（中文/全角算2，其余算1）"""
    w = 0
    for c in s:
        w += 2 if ord(c) > 127 else 1
    return w

def pad(s, width):
    """按视觉宽度右填充空格"""
    return s + " " * max(0, width - visual_len(s))

def _make_row(c1, c2, c3, c4, c5):
    cols = [c1, c2, c3, c4, c5]
    return "  ".join(pad(c, w) for c, w in zip(cols, COL_WIDTHS))

HEADER_ROW = _make_row("股票名/市值", "今日盈亏¥/%", "成本/现价(涨幅)", "总盈亏¥/%", "持仓/可用")

def print_menu():
    status = trading_status()
    info   = load_info()
    _clean_operate(info)      # 清除非今日操作记录
    hmap   = get_holdings(info)
    qmap   = get_quotes(info)

    # 周末
    if status == "weekend":
        print("Freedom")
        print("---")
        if hmap:
            print(f"{HEADER_ROW} | {FONT}")
            _render_static(hmap, qmap, info)
        _print_buttons()
        return

    # 无持仓
    if not hmap:
        print("Working")
        print("---")
        _print_buttons()
        return

    # 非交易时段：直接读 quote，不请求接口
    if status == "closed":
        print("Working")
        print("---")
        print(f"{HEADER_ROW} | {FONT}")
        _render_static(hmap, qmap, info)
        _print_buttons()
        return

    # 午休（11:30–12:59）或收盘时段（15:00–15:59）：检查 quote 是否已有当段数据
    if status in ("closing_noon", "closing"):
        today = date.today().isoformat()
        threshold = today + (" 11:30" if status == "closing_noon" else " 15:00")
        has_today = bool(qmap) and all(q.get("date", "") >= threshold for q in qmap.values())
        if has_today:
            print("Working")
            print("---")
            print(f"{HEADER_ROW} | {FONT}")
            _render_static(hmap, qmap, info)
            _print_buttons()
            return
        # 没有今日数据，拉取实时行情并写入
        try:
            quotes = fetch_quotes(list(hmap.keys()))
        except Exception:
            print("Err")
            print("---")
            _print_buttons()
            return
        session = "noon" if status == "closing_noon" else "afternoon"
        write_closing_quote(info, quotes, session=session)
        info = load_info()
        qmap = get_quotes(info)
        print("Working")
        print("---")
        print(f"{HEADER_ROW} | {FONT}")
        _render_static(hmap, qmap, info)
        _print_buttons()
        return

    # 16:00+ 纯展示缓存
    if status == "after_close":
        print("Working")
        print("---")
        if qmap:
            print(f"{HEADER_ROW} | {FONT}")
            _render_static(hmap, qmap, info)
        _print_buttons()
        return

    # 交易时段（open / closing）：实时拉行情，忽略 quote
    try:
        quotes = fetch_quotes(list(hmap.keys()))
    except Exception:
        print("Err")
        print("---")
        _print_buttons()
        return

    _render(hmap, quotes, info)
    _print_buttons()


def _render(hmap, quotes, info=None):
    if info is None:
        info = load_info()
    op_map = get_today_operate(info)
    total_day_pnl = 0.0
    rows = []
    for code, h in hmap.items():
        q = quotes.get(code)
        if not q:
            continue
        shares     = h["lots"] * lot_size(code)
        cost       = h["cost"]
        price      = q["price"]
        market_val = price * shares
        prev_close = q.get("prev_close") or price

        # 今日行情盈亏 + 操作盈亏
        day_pnl_market = q["change"] * shares
        op             = op_map.get(code)
        op_pnl         = op["pnl"] if op else 0.0
        day_pnl        = day_pnl_market + op_pnl

        # 当日盈亏比：分母 = 昨收 × 开盘持仓股数
        open_lots   = op.get("open_lots", h["lots"]) if op else h["lots"]
        open_shares = open_lots * lot_size(code)
        day_base    = prev_close * open_shares
        day_pct     = (day_pnl / day_base * 100) if day_base else 0

        # 总盈亏 = 浮动 + 所有操作盈亏
        all_op_pnl = sum(o["pnl"] for o in info.get("operate", []) if o["code"] == code)
        total_pnl  = (price - cost) * shares + all_op_pnl
        # lots=0 时用开盘持仓手数作为成本基准（防止除零）
        cost_lots  = shares if shares > 0 else open_shares
        total_cost = cost * cost_lots
        total_pct  = (total_pnl / total_cost * 100) if total_cost else 0

        # 行情涨幅
        price_pct  = q.get("pct", 0.0)

        total_day_pnl += day_pnl
        rows.append((market_val, h["name"], day_pnl, day_pct, cost, price, price_pct,
                     total_pnl, total_pct, h["lots"], h["available_lots"]))

    rows.sort(key=lambda r: r[0], reverse=True)
    print(f"{fmt(total_day_pnl)}")
    print("---")
    print(f"{HEADER_ROW} | {FONT}")
    for r in rows:
        market_val, name = r[0], r[1]
        name_mv = f"{name}/{market_val:.0f}"
        print(f"{_row(name_mv, *r[2:])} | {FONT}")


def _render_static(hmap, qmap, info=None):
    """非交易时段：从 quote 缓存渲染，无行情显示 -"""
    if info is None:
        info = load_info()
    op_map = get_today_operate(info)
    print("---")
    rows = []
    for code, h in hmap.items():
        q = qmap.get(code)
        if q:
            market_val = q.get("market_val", q["price"] * h["lots"] * lot_size(code))
            rows.append((market_val, h, q))
        else:
            rows.append((0.0, h, None))
    rows.sort(key=lambda r: r[0], reverse=True)
    for market_val, h, q in rows:
        if q:
            code   = h["code"]
            shares = h["lots"] * lot_size(code)
            cost   = h["cost"]
            price  = q["price"]

            op      = op_map.get(code)
            op_pnl  = op["pnl"] if op else 0.0
            day_pnl = q["day_pnl"] + op_pnl

            # 当日盈亏比：分母 = 昨收价 × 开盘持仓股数
            # 静态缓存没有昨收，用缓存价格兜底
            open_lots   = op.get("open_lots", h["lots"]) if op else h["lots"]
            open_shares = open_lots * lot_size(code)
            day_base    = price * open_shares   # 静态模式用缓存价代替昨收
            day_pct     = (day_pnl / day_base * 100) if day_base else q["day_pct"]

            all_op_pnl = sum(o["pnl"] for o in info.get("operate", []) if o["code"] == code)
            total_pnl  = q["total_pnl"] + all_op_pnl
            # lots=0 时用开盘持仓手数作为成本基准（防止除零）
            cost_shares = shares if shares > 0 else open_shares
            total_cost = cost * cost_shares
            total_pct  = (total_pnl / total_cost * 100) if total_cost else q["total_pct"]

            price_pct = q.get("day_pct", 0.0)   # 缓存里的行情涨幅

            name_mv = f"{h['name']}/{market_val:.0f}"
            line = _row(name_mv, day_pnl, day_pct, cost, price, price_pct,
                        total_pnl, total_pct, h["lots"], h["available_lots"])
        else:
            line = _make_row(h["name"], "-", f"{h['cost']:.2f}/-", "-",
                             f"{h['lots']:.0f}/{h['available_lots']:.0f}")
        print(f"{line} | {FONT}")


def _row(name, day_pnl, day_pct, cost, price, price_pct, total_pnl, total_pct, lots, avail):
    return _make_row(
        name,
        f"{fmt(day_pnl)}/{fmt(day_pct)}%",
        f"{cost:.2f}/{price:.2f}({fmt(price_pct)}%)",
        f"{fmt(total_pnl)}/{fmt(total_pct)}%",
        f"{lots:.0f}/{avail:.0f}",
    )


def _print_buttons():
    script = os.path.abspath(__file__)
    print("---")
    print(f"初始化持仓 | bash={script} param1=init terminal=false refresh=true")
    print(f"加仓       | bash={script} param1=buy  terminal=false refresh=true")
    print(f"减仓       | bash={script} param1=sell terminal=false refresh=true")


# ── 入口 ──────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd in ("init", "buy", "sell"):
            run_form(cmd)
    else:
        print_menu()
