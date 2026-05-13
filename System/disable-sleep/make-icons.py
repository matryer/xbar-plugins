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
# templateImage requirements (per xbar): only black + transparent pixels;
# macOS recolors based on menubar foreground (white in dark mode, black in light).
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
    from PIL import Image, ImageDraw
except ImportError:
    if __name__ == "__main__":
        _bootstrap_with_uv()
    else:
        raise
### END UV BOOTSTRAP ###


# Canvas size. 72x72 at 144 DPI = ~36 logical points,
# which downscales cleanly to the macOS menubar height.
W = H = 72
BLACK = (0, 0, 0, 255)


def base_bed(d: "ImageDraw.ImageDraw") -> None:
    """Side-view bed silhouette. Filled black on transparent."""
    # headboard (tall vertical bar, left)
    d.rectangle([6, 14, 16, 58], fill=BLACK)
    # pillow sitting on mattress, with a small visual gap from headboard
    d.rounded_rectangle([20, 30, 36, 44], radius=4, fill=BLACK)
    # mattress (slightly thicker for chunkier silhouette)
    d.rectangle([18, 46, 66, 58], fill=BLACK)
    # bed frame under mattress (joins headboard to right side)
    d.rectangle([6, 58, 66, 62], fill=BLACK)
    # legs
    d.rectangle([10, 62, 16, 68], fill=BLACK)
    d.rectangle([56, 62, 62, 68], fill=BLACK)


def render(path: str, *, slash: bool) -> None:
    img = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    base_bed(d)
    if slash:
        # Diagonal "no" slash, top-right to bottom-left.
        # Drawn last so it overlays the bed.
        d.line([(64, 6), (6, 64)], fill=BLACK, width=10)
    img.save(path, format="PNG", dpi=(144, 144))


def main() -> None:
    here = os.path.dirname(os.path.realpath(__file__))
    render(os.path.join(here, "bed.png"),    slash=False)
    render(os.path.join(here, "bed-no.png"), slash=True)
    print(f"Wrote bed.png and bed-no.png to {here}")


if __name__ == "__main__":
    main()
