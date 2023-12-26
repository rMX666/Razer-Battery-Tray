# Razbat

A simple tool to display battery level of Razer devices in system tray.
Works on Windows, may as well work on Linux, but there's OpenRazer anyway.

# Libraries

Project uses following libraries:
* pas-libusb - FPC adaper for libusb: https://github.com/hansiglaser/pas-libusb
* libusb.dll itself: https://libusb.info

# Supported devices

Verified:
* Razer Viper Ultimate
* Razer Deathadder v3 Pro
* Razer Mouse Dock (shows nothing, but I own it, so why not :)).

List of supported devices may be extended by adding new devices to bin/devices.json.

Product ID may be found in OpenRazer header files, for example, supported mice: https://github.com/openrazer/openrazer/blob/master/driver/razermouse_driver.h

Transaction ID: https://github.com/openrazer/openrazer/blob/master/driver/razermouse_driver.c#L1132
