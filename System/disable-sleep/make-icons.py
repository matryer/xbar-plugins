#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = ["pillow==10.3.0"]
# ///

# Generator for the two menubar templateImages used by disable-sleep.10s.sh.
#
# - bed.png      : plain bed
# - bed-no.png   : bed + slash
#
# Silhouettes match origin/main rectangles. The UR slash endpoint is eased
# slightly toward UR–mid (shorter poke into the corner). The whole bed shifts
# up a hair. Raster bbox of bed ∪ eased-slash defines the geometric centre,
# plus a tiny extra vertical offset (applied *identically* for both PNGs) so ink
# weight reads centred — alpha centroid tracked at ~Δy +0.4 vs dead centre vs
# +3 with bbox-only balancing.
#
# Run:
#     uv run System/disable-sleep/make-icons.py
# or: python3 System/disable-sleep/make-icons.py

import os
import sys

### BEGIN UV BOOTSTRAP ###
def _bootstrap_with_uv():
    uv_paths = [
        os.path.expanduser("~/.local/bin/uv"),
        os.path.expanduser("~/.cargo/bin/uv"),
        "/opt/homebrew/bin/uv",
        "/usr/local/bin/uv",
    ]
    script_path = os.path.realpath(__file__)
    for uv in uv_paths:
        if os.path.isfile(uv):
            os.execv(uv, [uv, "run", script_path])
    print("ERR: pillow not installed. Tried bootstrapping with uv, but uv not found.")
    print("Install uv: curl -LsSf https://astral.sh/uv/install.sh | sh")
    print("Or install pillow directly: python3 -m pip install pillow")
    sys.exit(1)

try:
    from PIL import Image, ImageChops, ImageDraw
except ImportError:
    if __name__ == "__main__":
        _bootstrap_with_uv()
    else:
        raise
### END UV BOOTSTRAP ###


OUT = 44                  # final image size in pixels
SS = 8                    # supersampling factor
W = H = OUT * SS          # supersampled canvas size
BLACK = (0, 0, 0, 255)

# Canonical diagonal runs (40,4) → (4,40). Slide the UR anchor along the chord
# toward the segment midpoint (~22,22): 0 keeps full corner poke, ↑ shortens UR.
_SLASH_PULL_UR_FRACTION = 0.17

# Subtract from every rect *y in output space — nudges the bed glyph upward (~1¼ px).
_BED_VERTICAL_OFFSET_OUT = -1.25

# Tiny pan after bbox-centring (same for bed + crossed). Supersampled pixels;
# -13 SS ≈ 1.6 output px ↑ — balances bbox vs heavier bed pixels (inspector).
_BIAS_TY_SUPER = -13


def _r(coords):
    """Scale a list of output-pixel coords up to the supersampled canvas."""
    return [c * SS for c in coords]


def slash_endpoints() -> tuple[float, float, float, float]:
    """UR→LL slash; UR only is pulled toward midpoint (shorter corner arm)."""
    mid = OUT / 2.0
    ur_x, ur_y = float(OUT) - 4.0, 4.0
    ll_x, ll_y = 4.0, float(OUT) - 4.0
    t = _SLASH_PULL_UR_FRACTION
    x0 = ur_x + t * (mid - ur_x)
    y0 = ur_y + t * (mid - ur_y)
    return x0, y0, ll_x, ll_y


def base_bed(d: "ImageDraw.ImageDraw") -> None:
    """Origin/main rectangles; optional vertical nudge upward."""
    dy = _BED_VERTICAL_OFFSET_OUT
    # headboard (tall left post)
    d.rounded_rectangle(_r([5,  13 + dy, 10, 33 + dy]), radius=1.5 * SS, fill=BLACK)
    d.rounded_rectangle(_r([13, 19 + dy, 21, 25 + dy]), radius=2.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([11, 24 + dy, 38, 32 + dy]), radius=2.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([5,  32 + dy, 40, 35 + dy]), radius=1.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([7,  34 + dy, 10, 38 + dy]), radius=1.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([35, 34 + dy, 38, 38 + dy]), radius=1.0 * SS, fill=BLACK)


def add_slash(img: "Image.Image") -> "Image.Image":
    """Same thickness / gap as origin; eased UR anchor only."""
    slash_w = 4
    gap_w = slash_w + 2
    x0, y0, x1, y1 = slash_endpoints()

    erase = Image.new("L", img.size, 0)
    ed = ImageDraw.Draw(erase)
    ed.line(_r([x0, y0, x1, y1]), fill=255, width=int(gap_w * SS))
    gh = gap_w / 2
    for cx, cy in [(x0, y0), (x1, y1)]:
        ed.ellipse(_r([cx - gh, cy - gh, cx + gh, cy + gh]), fill=255)

    r, g, b, a = img.split()
    a = ImageChops.multiply(a, ImageChops.invert(erase))
    img = Image.merge("RGBA", (r, g, b, a))

    d = ImageDraw.Draw(img)
    d.line(_r([x0, y0, x1, y1]), fill=BLACK, width=int(slash_w * SS))
    sh = slash_w / 2
    for cx, cy in [(x0, y0), (x1, y1)]:
        d.ellipse(_r([cx - sh, cy - sh, cx + sh, cy + sh]), fill=BLACK)

    return img


def compose_layer(with_slash: bool) -> "Image.Image":
    layer = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    base_bed(ImageDraw.Draw(layer))
    if with_slash:
        layer = add_slash(layer)
    return layer


def rigid_center_shift() -> tuple[int, int]:
    """Centre bbox(bed ∪ slash) on supersampled canvas (slash = eased UR)."""
    layer = compose_layer(with_slash=True)
    bbox = layer.getbbox()
    if bbox is None:
        return (0, 0)
    left, upper, right, lower = bbox
    cx = (left + right) / 2.0
    cy = (upper + lower) / 2.0
    return int(round(W / 2 - cx)), int(round(H / 2 - cy))


def render(path: str, *, with_slash: bool) -> None:
    tx, ty = rigid_center_shift()
    ty += _BIAS_TY_SUPER
    canvas = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    canvas.alpha_composite(compose_layer(with_slash), (tx, ty))
    img = canvas.resize((OUT, OUT), Image.LANCZOS)
    img.save(path, format="PNG", dpi=(144, 144))


def main() -> None:
    here = os.path.dirname(os.path.realpath(__file__))
    render(os.path.join(here, "bed.png"), with_slash=False)
    render(os.path.join(here, "bed-no.png"), with_slash=True)
    print(f"Wrote bed.png and bed-no.png to {here}")
    print(
        f"(inspect tweak: UR_pull={_SLASH_PULL_UR_FRACTION} bed_dy_out={_BED_VERTICAL_OFFSET_OUT} bias_ty_SS={_BIAS_TY_SUPER})"
    )


if __name__ == "__main__":
    main()
