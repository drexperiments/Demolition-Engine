// i'mma have the songs separate in 2 different menus from here
package;

#if desktop
import Discord.DiscordClient;
#end
import editors.ChartingState;
import flash.text.TextField;
import flixel.FlxG;
import flixel.FlxSprite;
import flixel.addons.display.FlxGridOverlay;
import flixel.addons.transition.FlxTransitionableState;
import flixel.group.FlxGroup.FlxTypedGroup;
import flixel.math.FlxMath;
import flixel.text.FlxText;
import flixel.util.FlxColor;
import flixel.tweens.FlxEase;
import flixel.tweens.FlxTween;
import lime.utils.---ets;
import flixel.system.FlxSound;
import openfl.utils.---ets as OpenFl---ets;
import WeekData;
#if MODS_ALLOWED
import sys.FileSystem;
#end

using StringTools;
//just a copy from FreeplayState lol

cl--- FreeplayCategoryState extends MusicBeatState
{
        var vanillaGame:FlxSprite;
        var exclusive----:FlxSprite;
        var modsMusic:FlxSprite;
        var selectorBox:FlxSprite;
  
        var bg:FlxSprite;
	      public var camZooming:Bool = false;
  
        override function create()
      	{
                Paths.clearStoredMemory();
		            Paths.clearUnusedMemory();
		
		            persistentUpdate = true;

		            #if desktop
		            // Updating Discord Rich Presence
		            DiscordClient.changePresence("In Freeplay Category", null);
		            #end
                  
                bg = new FlxSprite().loadGraphic(Paths.image('menuFreeplayCate'));
		bg.antialiasing = ClientPrefs.globalAntialiasing;
                bg.color = 0xFF424d54;
		add(bg);
		bg.screenCenter();
          
                exclusive---- = new FlxSprite().loadGraphic(Paths.image('bonusBanner'));
                exclusive----.antialiasing = ClientPrefs.globalAntialiasing;
                add(exclusive----);
          
                modsMusic = new FlxSprite().loadGraphic(Paths.image('modsMenuBanner'));
                modsMusic.antialiasing = ClientPrefs.globalAntialiasing;
                add(modsMusic);
          	
		vanillaGame = new FlxSprite().loadGraphic(Paths.image('mainGameBanner'));
                vanillaGame.antialiasing = ClientPrefs.globalAntialiasing;
                add(vanillaGame);
		
                selectorBox = new FlxSprite().loadGraphic(Paths.image('daSelecta'));
                selectorBox.antialiasing = ClientPrefs.globalAntialiasing;
                add(selectorBox);
          
                super.create();
        }
}
//We workin' on this, clearly not finished lmao
