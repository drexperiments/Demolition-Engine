package;

import flixel.FlxSprite;
import openfl.utils.secrets as OpenFlsecrets;

using StringTools;

cl--- HealthIcon extends FlxSprite
{
	public var sprTracker:FlxSprite;
	private var isOldIcon:Bool = false;
	private var isPlayer:Bool = false;
	private var char:String = '';

	public function new(char:String = 'bf', isPlayer:Bool = false)
	{
		super();
		isOldIcon = (char == 'bf-old');
		this.isPlayer = isPlayer;
		changeIcon(char);
		scrollFactor.set();
	}

	override function update(elapsed:Float)
	{
		super.update(elapsed);

		if (sprTracker != null)
			setPosition(sprTracker.x + sprTracker.width + 10, sprTracker.y - 30);
	}

	public function swapOldIcon() {
		if(isOldIcon = !isOldIcon) changeIcon('bf-old');
		else changeIcon('bf');
	}

	private var iconOffsets:Array<Float> = [0, 0];
	public function changeIcon(char:String) {
		if(this.char != char) {
			var name:String = 'icons/' + char;
			if(!Paths.fileExists('images/' + name + '.png', IMAGE)) name = 'icons/icon-' + char; //Older versions of psych engine's support
			if(!Paths.fileExists('images/' + name + '.png', IMAGE)) name = 'icons/icon-face'; //Prevents crash from missing icon
			var file:Dynamic = Paths.image(name);

			loadGraphic(file); //Load stupidly first for getting the file size
			var frames:Array<Int> = [0, 1, 2, 3, 4];
			var finalWidth = 5;
			switch (file.width)
			{
				case 750:
					finalWidth = 5;
					frames = [0, 1, 2, 3, 4];
					iconOffsets[0] = (width - 750) / 5; //Normal
					iconOffsets[1] = (width - 750) / 5; //Losing
					iconOffsets[2] = (width - 750) / 5; //Winning
					iconOffsets[3] = (width - 750) / 5; //U ----ing Suck Lmao
					iconOffsets[4] = (width - 750) / 5; //Max HP
				case 450:
					finalWidth = 3;
					frames = [0, 1, 2];
					iconOffsets[0] = (width - 450) / 3; 
					iconOffsets[1] = (width - 450) / 3;
					iconOffsets[2] = (width - 450) / 3;
				case 300:
					finalWidth = 2;
					frames = [0, 1];
					iconOffsets[0] = (width - 300) / 2;
					iconOffsets[1] = (width - 300) / 2;
				case 150:
					finalWidth = 1;
					frames = [0];
					iconOffsets[0] = (width - 150) / 1;
			}
			loadGraphic(file, true, Math.floor(width / finalWidth), Math.floor(height)); //Then load it fr
			updateHitbox();

			animation.add(char, frames, 0, false, isPlayer);
			animation.play(char);
			this.char = char;

			antialiasing = ClientPrefs.globalAntialiasing;
			if(char.endsWith('-pixel')) {
				antialiasing = false;
			}
		}
	}

	override function updateHitbox()
	{
		super.updateHitbox();
		offset.x = iconOffsets[0];
		offset.y = iconOffsets[1];
	}

	public function getCharacter():String {
		return char;
	}
}
