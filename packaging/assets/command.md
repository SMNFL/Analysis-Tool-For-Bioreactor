## resize dmg background

```bash
sips -z 800 1600 packaging/assets/dmg_background.png --out temp.png
sips -z 400 800 temp.png --out packaging/assets/dmg_background_resized.png
rm temp.png
````

```bash
sips -z 800 1600 packaging/assets/dmg_background_blank.png --out temp.png
sips -z 400 800 temp.png --out packaging/assets/dmg_background_blank_resized.png
rm temp.png
````
