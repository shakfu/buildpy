# Development Notes

## Weird Bugs

- If `_decimal` is enabled in static or shared, `makesetup` erroneously includes it in the `Makefile` and causes the build to fail. Disabling `_decimal` fixes this on 3.11, however it does not work for 3.12
