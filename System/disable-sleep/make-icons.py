#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = ["pillow==10.3.0"]
# ///

# Generator for the two menubar templateImages used by disable-sleep.10s.sh.
#
# - bed.png    : "sleep allowed" state (plain bed)
# - bed-no.png : "sleep denied" state (bed with diagonal slash)
#
# Only black + transparent pixels — macOS recolours templates for the menu bar.
# Output 44×44 @ 144 DPI (~22 pt natural menubar icon). Drawn at 8× supersampling
# then LANCZOS downsampled for clean edges.
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
W = H = OUT * SS
BLACK = (0, 0, 0, 255)


def _r(coords):
    return [c * SS for c in coords]


def base_bed(d: "ImageDraw.ImageDraw") -> None:
    """Side-view bed, ~10% inset from the old layout: centred on (22, 22), no
    thin frame strip (legs attach under the mattress) so the front edge stays
    clean after downsample.
    """
    # headboard (left vertical bar)
    d.rounded_rectangle(_r([12, 15, 15, 28]), radius=1.5 * SS, fill=BLACK)
    # pillow
    d.rounded_rectangle(_r([16, 18, 23, 22]), radius=1.8 * SS, fill=BLACK)
    # mattress
    d.rounded_rectangle(_r([14, 22, 30, 26]), radius=1.8 * SS, fill=BLACK)
    # legs — directly under mattress
    d.rounded_rectangle(_r([16, 26, 18, 30]), radius=1.0 * SS, fill=BLACK)
    d.rounded_rectangle(_r([26, 26, 28, 30]), radius=1.0 * SS, fill=BLACK)


def add_slash(img: "Image.Image") -> "Image.Image":
    """Shorter diagonal through the canvas centre (not corner-to-corner)."""
    slash_w = 2.5            # output px — thinner than the old 4 px line
    gap_w = slash_w + 2.0
    # Line through (22, 22), slope -1, ~9 px half-length along the diagonal
    x0, y0 = 28.5, 15.5
    x1, y1 = 15.5, 28.5

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


def render(path: str, *, with_slash: bool) -> None:
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    base_bed(ImageDraw.Draw(img))
    if with_slash:
        img = add_slash(img)
    img = img.resize((OUT, OUT), Image.LANCZOS)
    img.save(path, format="PNG", dpi=(144, 144))


def main() -> None:
    here = os.path.dirname(os.path.realpath(__file__))
    render(os.path.join(here, "bed.png"), with_slash=False)
    render(os.path.join(here, "bed-no.png"), with_slash=True)
    print(f"Wrote bed.png and bed-no.png to {here}")


if __name__ == "__main__":
    main()
