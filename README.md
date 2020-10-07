# swd_ch_lineups
You need to clone this repository to your working directory:<br>
https://github.com/fafranco82/swdestinydb-json-data<br>
Then download my scripts.<br><br>
If you want to score characters and build automatic lineups, you want json_swdb.R.<br>
You'll need to edit line 7 of this script to the location of the 'set' folder from the swdb repository.<br>
You can edit the scoring system from line 19.<br>
You can edit the modifier downgrade value in line 24 (e.g a +2 Ranged is worth 0.7 times the value of a 2 Ranged base side)<br>
You might want to also edit line 27 and choose which sets you want to build with.  These use the swdb abbreviations:<br>
AW, SoR, EaW, TPG, LEG, WotF, RIV, AtG, AoN, CONV, SoH, CM, TR<br>
Just message me if you have trouble.<br><br>
If you want to create a spreadsheet of all cards released to date, you want create_all_card_data.R.<br>
You'll need to edit line 1 of this script to the location of the 'set' folder from the swdb repository.<br><br>
Finally, format_csv.R is an attempt to convert a spreadsheet into json format (the inverse of the create_all_card_data.R script.<br>
The FA.csv sheet is a source csv for this script to run on.
