#!/usr/bin/env python3
# /// script
# requires-python = ">=3.9"
# dependencies = ["pillow==10.3.0"]
# ///

# Generator for the three menubar icons used by disable-sleep.10s.sh.
#
# - bed.png           : "sleep allowed" state. Black on transparent. Used as
#                       xbar templateImage so macOS recolours it per
#                       appearance (white on dark mode, black on light).
# - bed-no-light.png  : "sleep denied" state in light system appearance.
#                       Black bed + red slash. Used as xbar image= (NOT
#                       templateImage) so the red survives.
# - bed-no-dark.png   : "sleep denied" state in dark system appearance.
#                       White bed + red slash. Also used as image=.
#
# Output is 18x18 raw pixels with no DPI metadata, so xbar renders the icon
# at ~18pt regardless of whether it honours pHYs. Drawn at 16x supersampling
# and downsampled with LANCZOS for clean anti-aliased edges.
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


OUT = 18                  # final image size in pixels (no DPI -> 18pt natural)
SS = 16                   # supersampling factor
W = H = OUT * SS          # supersampled canvas size

BLACK = (0, 0, 0, 255)
WHITE = (255, 255, 255, 255)
RED = (0xFF, 0x3B, 0x30, 255)   # macOS systemRed / destructive-action red


def _r(coords):
    """Scale a list of output-pixel coords up to the supersampled canvas."""
    return [c * SS for c in coords]


def base_bed(d: "ImageDraw.ImageDraw", fg) -> None:
    """Side-view bed silhouette, sketched in 18-px output coordinates.

    The bed is vertically centred around y=10 (canvas centre is y=9; the
    pillow on top of the mattress pulls the visual mass slightly toward
    the top so we centre the silhouette around 10 to compensate).

    All coords pass through _r() so the draw happens on the supersampled
    canvas; the final downsample handles anti-aliasing.
    """
    # headboard (vertical bar, left, with rounded top)
    d.rounded_rectangle(_r([2,  5,  4, 13]), radius=0.8 * SS, fill=fg)
    # pillow at the head end of the mattress
    d.rounded_rectangle(_r([5,  7,  9, 10]), radius=0.6 * SS, fill=fg)
    # mattress (rounded rect; legs attach to its underside, no separate frame)
    d.rounded_rectangle(_r([4,  10, 16, 13]), radius=0.8 * SS, fill=fg)
    # legs (directly under the mattress)
    d.rounded_rectangle(_r([5,  13,  7, 15]), radius=0.4 * SS, fill=fg)
    d.rounded_rectangle(_r([13, 13, 15, 15]), radius=0.4 * SS, fill=fg)


def add_slash(img: "Image.Image", slash_color) -> "Image.Image":
    """Punch a transparent stripe through `img` along the no-slash diagonal,
    then draw the slash itself. The gap keeps the bed and the slash from
    visually mashing together.

    Both stripe and slash carry round end-caps (Pillow's `line` draws butt
    caps by default; we add ellipses to mimic round caps).
    """
    slash_w = 3              # output px, slash thickness
    gap_w = slash_w + 1.5    # output px, transparent gap around the slash
    x0, y0 = 15.5, 2.5       # slash start (upper-right)
    x1, y1 = 2.5, 15.5       # slash end   (lower-left)

    # Punch out the wider stripe + matching round caps on the alpha channel.
    erase = Image.new("L", img.size, 0)
    ed = ImageDraw.Draw(erase)
    ed.line(_r([x0, y0, x1, y1]), fill=255, width=int(gap_w * SS))
    gh = gap_w / 2
    for cx, cy in [(x0, y0), (x1, y1)]:
        ed.ellipse(_r([cx - gh, cy - gh, cx + gh, cy + gh]), fill=255)

    r, g, b, a = img.split()
    a = ImageChops.multiply(a, ImageChops.invert(erase))
    img = Image.merge("RGBA", (r, g, b, a))

    # Draw the slash itself (with matching round caps).
    d = ImageDraw.Draw(img)
    d.line(_r([x0, y0, x1, y1]), fill=slash_color, width=int(slash_w * SS))
    sh = slash_w / 2
    for cx, cy in [(x0, y0), (x1, y1)]:
        d.ellipse(_r([cx - sh, cy - sh, cx + sh, cy + sh]), fill=slash_color)

    return img


def render(path: str, *, bed_fg, with_slash: bool) -> None:
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    base_bed(ImageDraw.Draw(img), bed_fg)
    if with_slash:
        img = add_slash(img, RED)
    img = img.resize((OUT, OUT), Image.LANCZOS)
    # No DPI passed -> Pillow writes no pHYs chunk; the image renders at
    # its raw 18-px size no matter how the consumer handles DPI metadata.
    img.save(path, format="PNG")


def main() -> None:
    here = os.path.dirname(os.path.realpath(__file__))
    # Template image for the "allowed" state — macOS recolours it itself.
    render(os.path.join(here, "bed.png"),          bed_fg=BLACK, with_slash=False)
    # Non-template images for the "denied" state — bed colour is baked in
    # per appearance because non-templates aren't recoloured by macOS.
    render(os.path.join(here, "bed-no-light.png"), bed_fg=BLACK, with_slash=True)
    render(os.path.join(here, "bed-no-dark.png"),  bed_fg=WHITE, with_slash=True)
    print(f"Wrote bed.png, bed-no-light.png, bed-no-dark.png to {here}")


if __name__ == "__main__":
    main()
