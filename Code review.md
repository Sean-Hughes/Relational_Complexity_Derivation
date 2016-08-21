# Code review round 1

## Resolved issues

The IATs countervalance block order based on the participant code/2. The batch file already splits conditions by participant code/2, so condition and IAT block order are confounded. My notes say the agreed block design was to use a fixed IAT block order, so I've removed these from the IAT scripts

The coloring of the Test IAT was not identical to the Baseline IAT (i.e., "or" was in green instead of the attribute labels). I've fixed this by duplicating the Baseline IAT code and changing only the stimuli. Using identical code where possible would be preferable.

## Unresolved issues

I don't know inquisit well enough yet to immediately spot the souces of these issues, so I haven't resolved them. 

The fourth IAT block presented is bock 5, i.e. it uses only the attribute stimuli. However, instructions appear to be from block 4. There's some form of block ordering issue. The total number of blocks isn't what it should be from what I can see.

Blocks 1 and 2 contain only 7 stimuli rather than 8.

## Broader suggestions

Authorship, copyrighting and licensing of each script should be clarified at the top of each file and/or in the readme. Some are (c) millisecond, but altered. Some note you as author, but don't note a copyright and no licences are noted. Many people won't care much, but it makes people life easier if you just slap a GPL3+ notice on the readme file (with exceptions, e.g., for the millisecond scripts). This way, if someone wants to use the code for replication/extension there's no doubt that they're entitled to do so. It also contains a good default warning that you provide no guarantee. And, of course, you also thereby require anyone who uses your code to make it available too, so that you can benefit in the future. See my github repos for license files and notices in readmes.



