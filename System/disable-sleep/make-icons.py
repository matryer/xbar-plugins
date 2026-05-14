#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = ["pillow==10.3.0"]
# ///

# Generator for the two menubar templateImages used by disable-sleep.10s.sh.
#
# - bed.png      : "sleep allowed" state (plain bed)
# - bed-no.png   : "sleep denied" state  (bed with diagonal slash)
#
# Drawing matches origin/main bed + slash (corner-to-corner diagonal, same
# thickness and gap). Before downsample, one rigid `(tx, ty)` is computed from
# the **alpha bbox of bed ∪ slash** so the crossed-out glyph is centred on the
# 44² canvas; both PNGs use that same shift so toggling stays aligned.
#
# Run with:
#     uv run System/disable-sleep/make-icons.py
# or, if pillow is already installed:
#     python3 System/disable-sleep/make-icons.py

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


def _r(coords):
    """Scale a list of output-pixel coords up to the supersampled canvas."""
    return [c * SS for c in coords]


def base_bed(d: "ImageDraw.ImageDraw") -> None:
    """Side-view bed silhouette — same rectangles as origin/main.

    Coordinates stay in legacy output space; centre shift applies afterwards.
    """
    # headboard (tall left post)
    d.rounded_rectangle(_r([5,  13, 10, 33]), radius=1.5 * SS, fill=BLACK)
    # pillow at the head end of the mattress
    d.rounded_rectangle(_r([13, 19, 21, 25]), radius=2.0 * SS, fill=BLACK)
    # mattress
    d.rounded_rectangle(_r([11, 24, 38, 32]), radius=2.0 * SS, fill=BLACK)
    # frame (thin bar under mattress, joining headboard to right side)
    d.rounded_rectangle(_r([5,  32, 40, 35]), radius=1.0 * SS, fill=BLACK)
    # legs
    d.rounded_rectangle(_r([7,  34, 10, 38]), radius=1.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([35, 34, 38, 38]), radius=1.0 * SS, fill=BLACK)


def add_slash(img: "Image.Image") -> "Image.Image":
    """Exactly origin/main: gap + round caps + full-corner diagonal UR↔LL."""
    slash_w = 4              # output px (line thickness)
    gap_w = slash_w + 2      # output px (transparent stripe through bed)
    x0, y0 = 40, 4           # slash start (upper-right)
    x1, y1 = 4, 40           # slash end   (lower-left)

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


def rigid_center_shift() -> tuple[int, int]:
    """Rigid pan so bbox(bed ∪ full slash) is centred on the supersampled tile."""
    layer = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    base_bed(ImageDraw.Draw(layer))
    layer = add_slash(layer)
    bbox = layer.getbbox()
    if bbox is None:
        return (0, 0)
    left, upper, right, lower = bbox
    cx = (left + right) / 2.0
    cy = (upper + lower) / 2.0
    return int(round(W / 2 - cx)), int(round(H / 2 - cy))


def render(path: str, *, with_slash: bool) -> None:
    tx, ty = rigid_center_shift()
    layer = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    base_bed(ImageDraw.Draw(layer))
    if with_slash:
        layer = add_slash(layer)
    canvas = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    canvas.alpha_composite(layer, (tx, ty))
    img = canvas.resize((OUT, OUT), Image.LANCZOS)
    img.save(path, format="PNG", dpi=(144, 144))


def main() -> None:
    here = os.path.dirname(os.path.realpath(__file__))
    render(os.path.join(here, "bed.png"), with_slash=False)
    render(os.path.join(here, "bed-no.png"), with_slash=True)
    print(f"Wrote bed.png and bed-no.png to {here}")


if __name__ == "__main__":
    main()
