Discordantly

A bot for helping discord users look each other's profiles up, and other Path of Exile related functionality. The bot will ignore any input in any channel not containg the word 'bot' so as not to spam up regular channels.

## Commands
 - !getHelp - lists commands and any other questionably helpful information
 - !setProfile - will get your own profile information, if you have one set
 - !setProfile @user '\[Profile Name]'
 - !getProfiles - list the stored usernames with their profile names and one character per user if it can pull it up
 - !src - provides a link to this repository
 - !getClass \[Passive tree url] - gets the class and ascendancy of a passive tree url
 - !getStat '\[stat]'  - gets the count of how many nodes it can find with the _stat_ text in them
 - !getStat '\[stat]' \[Passive tree url] - gets how many _stat_ nodes with that text in them are also in the given tree
 - !getSkills '\[skill name]' - gets the required level to use the skill
 - !getSkills \[Pastebin PoB url] - gets the active skill names in a build, with the level required to use them
 - !poelink - provides a link to a jsFiddle with lots of useful Path of Exile information and related links
 - !maidservice - clean up all the bot's text and user issued-commands (if the bot has permission to do so in that channel)
  
## Contributing
 - Compile and add the bot to your server - instructions for adding a discord bot: https://discordpy.readthedocs.io/en/rewrite/discord.html
 - And/Or contact me in game - copy and paste this into your game - `@whisper IveBeenImpaled Hi, I want to talk about your discord bot`
 
## Related Repositories
 - C# discord bot enabling library - https://github.com/RogueException/Discord.Net
 - Python - Path of Exile related non-bot code - https://github.com/FWidm/poe-profile/
 - C# Path of Exile discord bot (different focus and way more functionality thus far) - https://github.com/Kyle-Undefined/PoE-Bot/
 - C# Unexplored discord bot example - https://github.com/Yucked/CL4P-TP

These were used to assist in figuring out desired functionality and translated to F#
