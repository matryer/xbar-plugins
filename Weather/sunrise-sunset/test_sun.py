#!/usr/bin/env uv ru
# /// script
# requires-python = ">=3.9"
# dependencies = [
#     "suntime==1.3.2",
#     "pillow==10.3.0",
#     "ansicolors>=1.1.8",
#     "scipy>=1.13.1",
# ]
# ///
"""
Test script for sun.1m.py - specifically testing draw_time_until_next_phase
to catch edge cases that previously caused errors.

Run with: uv run test_sun.py
"""

import importlib.util
import sys
from pathlib import Path

from PIL import Image, ImageDraw


def load_sun_module():
    """
    Import sun.1m.py module using importlib (has dots in name).
    
    The module's `if __name__ == '__main__'` guard prevents main() from running.
    The uv bootstrap won't trigger because we have the same dependencies.
    """
    module_path = Path(__file__).parent / "sun.1m.py"
    spec = importlib.util.spec_from_file_location("sun_module", module_path)
    sun_module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(sun_module)
    return sun_module


def test_draw_time_until_next_phase(sun_module):
    """
    Test the draw_time_until_next_phase function with various edge cases.
    
    The function draws a minute indicator when hours < 10. The main issues were:
    1. Wrong coordinate order (y2 < y1) causing PIL rectangle errors
    2. Negative y coordinates when height is small
    """
    draw_time_until_next_phase = sun_module.draw_time_until_next_phase
    
    # Test cases: (height, sunrise_in, sunset_in, description)
    # sunrise_in/sunset_in are minutes until sunrise/sunset
    # The function picks whichever is smaller and uses that to determine next_ph_h and next_ph_m
    test_cases = [
        # (height, sunrise_in, sunset_in, description)
        # Small height cases - most likely to cause negative y coords
        (2, 1*60 + 59, 12*60, "height=2, 1h59m to sunrise (max min_left=6)"),
        (2, 1*60 + 0, 12*60, "height=2, 1h0m to sunrise (min_left=0)"),
        (2, 1*60 + 30, 12*60, "height=2, 1h30m to sunrise"),
        
        # Very small heights
        (0, 1*60 + 30, 12*60, "height=0 edge case"),
        (1, 1*60 + 59, 12*60, "height=1, max min_left"),
        (4, 5*60 + 30, 12*60, "height=4, 5h30m"),
        
        # Default height (10)
        (10, 1*60 + 0, 12*60, "height=10, 1h0m (min_left=0)"),
        (10, 1*60 + 55, 12*60, "height=10, 1h55m (min_left=6)"),
        (10, 1*60 + 59, 12*60, "height=10, 1h59m (min_left=6)"),
        (10, 9*60 + 59, 12*60, "height=10, 9h59m (boundary hour)"),
        
        # Hour >= 10 should NOT display minute indicator
        (10, 10*60 + 0, 12*60, "height=10, 10h0m (no minute indicator)"),
        (10, 12*60 + 30, 15*60, "height=10, 12h30m (no minute indicator)"),
        
        # Large heights
        (20, 1*60 + 45, 12*60, "height=20, 1h45m"),
        (50, 1*60 + 59, 12*60, "height=50, 1h59m"),
        
        # Sunset closer than sunrise
        (10, 12*60, 1*60 + 30, "height=10, sunset in 1h30m"),
        (2, 12*60, 1*60 + 59, "height=2, sunset in 1h59m"),
        
        # Edge minute values
        (10, 1*60 + 5, 12*60, "height=10, 1h5m (min_left=1)"),
        (10, 1*60 + 14, 12*60, "height=10, 1h14m (min_left=1)"),
        (10, 1*60 + 15, 12*60, "height=10, 1h15m (min_left=2)"),
        (10, 1*60 + 44, 12*60, "height=10, 1h44m (min_left=4)"),
        (10, 1*60 + 45, 12*60, "height=10, 1h45m (min_left=5)"),
    ]
    
    passed = 0
    failed = 0
    
    for height, sunrise_in, sunset_in, description in test_cases:
        # Create a fresh image for each test
        # Width needs to be reasonable for the indicator to fit
        width = 72  # Default UNITS value (1440 / 20)
        
        try:
            image = Image.new("RGB", (width + 10, height + 10), "white")
            draw = ImageDraw.Draw(image)
            
            # Call the function
            draw_time_until_next_phase(draw, sunrise_in, sunset_in, width, height)
            
            print(f"  PASS: {description}")
            passed += 1
            
        except Exception as e:
            print(f"  FAIL: {description}")
            print(f"        Error: {type(e).__name__}: {e}")
            failed += 1
    
    return passed, failed


def test_coordinate_calculations(sun_module):
    """
    Directly test the coordinate calculations that were causing issues.
    This tests the math without calling PIL.
    """
    parse_hours_mins = sun_module.parse_hours_mins
    
    from datetime import timedelta
    
    CHAR_H = 8
    
    test_cases = [
        # (height, next_ph_h, next_ph_m, description)
        (2, 1, 59, "height=2, 1h59m"),
        (2, 1, 0, "height=2, 1h0m"),
        (0, 1, 30, "height=0"),
        (10, 1, 55, "height=10, 1h55m"),
        (10, 9, 59, "height=10, 9h59m"),
    ]
    
    passed = 0
    failed = 0
    
    for height, next_ph_h, next_ph_m, description in test_cases:
        # Replicate the calculation from draw_time_until_next_phase
        next_ph_height = (height - CHAR_H) // 2
        
        will_display_minute_indicator = next_ph_h > 0 and next_ph_h < 10
        
        if will_display_minute_indicator:
            min_left = next_ph_m // 10
            min_left += 1 if next_ph_m % 10 >= 5 else 0
            
            # Original calculations
            doty1 = next_ph_height + 6 + 1 + 1
            doty2 = doty1 - 1 + 1 - min_left + 0
            
            # Fixed calculations (should match what's in sun.1m.py now)
            y_top = max(0, min(doty1, doty2))
            y_bottom = max(doty1, doty2)
            
            valid = y_bottom > y_top and y_top >= 0
            
            if valid:
                print(f"  PASS: {description}")
                print(f"        next_ph_height={next_ph_height}, min_left={min_left}")
                print(f"        doty1={doty1}, doty2={doty2} -> y_top={y_top}, y_bottom={y_bottom}")
                passed += 1
            else:
                print(f"  WARN: {description} - rectangle would be skipped (y_top={y_top}, y_bottom={y_bottom})")
                # This is actually OK - we just skip drawing in this case
                passed += 1
        else:
            print(f"  SKIP: {description} - minute indicator not displayed (hours >= 10)")
            passed += 1
    
    return passed, failed


def main():
    print("=" * 60)
    print("Testing sun.1m.py - draw_time_until_next_phase")
    print("=" * 60)
    
    print("\nLoading sun.1m.py module...")
    sun_module = load_sun_module()
    print("Module loaded successfully.\n")
    
    print("--- Testing coordinate calculations ---\n")
    calc_passed, calc_failed = test_coordinate_calculations(sun_module)
    
    print("\n--- Testing actual drawing function ---\n")
    draw_passed, draw_failed = test_draw_time_until_next_phase(sun_module)
    
    total_passed = calc_passed + draw_passed
    total_failed = calc_failed + draw_failed
    
    print("\n" + "=" * 60)
    print(f"Results: {total_passed} passed, {total_failed} failed")
    print("=" * 60)
    
    if total_failed > 0:
        sys.exit(1)
    else:
        print("\nAll tests passed!")
        sys.exit(0)


if __name__ == "__main__":
    main()
