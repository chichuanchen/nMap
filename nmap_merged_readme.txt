merged files contain behavioral data for all subjects (you'll need to check all are actually there) from e-prime.

"subject" column is subject number and should match "old" TRN number
"session" should be pre=1 or post=2 (but check original files if merged files aren't already separated correctly). [[checked]]

you should filter using these variables to only include participants with useable brain/nmap data in our ERP dataset. make sure you include session 1, session 2 or both and coordinate with the available good brain data.
[[filtered]]

"tag" is the last column and is the most important to coding this, as it has the number word quantity and then the visual quantity.
e.g., 103 means it said "one" and showed "3"
e.g., 201 means it said "two" and showed "1"

You can double check if you need with these columns:
"procedure[BLOCK]" just tells which animal was being presented---not important really.
"sound1[SubTrial]" tells what number was said includes .wav file played
"image[SubTrial]" tells which image was presented (includes number)

There were two events where the child could make a response.....either on the animal picture itself during the display delay before the next trial started.
So, you'll have to combine either a correct response for "animal pic" or a correct response during the "response display" as correct.
RT is not reliable so no need to calculate.

IF the .ACC variable is 1 for either then they got it correct (see below).
You can check to make sure the CRESP between "animalpic" and "respdisp" is the same--it should be. [[its not. so I used tag to produce my own correct resp]]

animalpic.ACC is whether they got it correct or not during the initial animal pic presentation
animalpic.CRESP is what the answer should have been i.e. the correct response (button 1 or 4)
animalpic.RESP is what they responded, i.e., their response (button 1 or 4 or nothing)

respdisp.ACC is whether they got it correct or not.
respdisp.CRESP is the response they should have made/i.e., the correct response. (button 1 or 4)
respdisp.RESP is the response they actually made, i.e., their response (button 1 or 4 or nothing)


