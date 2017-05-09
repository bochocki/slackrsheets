# slackrsheets
A hyper-specific R package for posting data to a Google Spreadsheet from Slack

# How to use this thing in Slack
To add your time for today, just enter your time: `/mini 1:42`

Your time can be in just about any form; 1:02, 62, and 00:01:02 all have the same effect. You can also log strings... `FAIL`, for example, is one string you might need to type in. Case isn't important, and special characters (other than `+` and `-`) should be fine. Be careful with spaces though; if you go too crazy they could break everything and it's best to avoid them.

To log somebody else's time, just use the `for` keyword:  
`/mini 0:39 for Brad`

Keep in mind that **posting times are subject to ECB (East-Coast Bias)**, so if you're on the West Coast and you're posting a time late at night, you might need to post the time for *yesterday*. Which you can totally do.

You can log past or future days using a `+` or `-` tag. So, logging a time for yesterday would look like:  
`/mini 42 -1`

and logging a time for tomorrow would look like:  
`/mini 42 +1`

You can time travel any number of days in any direction, so be careful not to get lost.

You can also overwrite times by including a `-ow` flag. To overwrite yesterday's time for example, you can use:  
`/mini 42 -1 -ow`

You can chain all of these things together too, the only important thing is to make sure that the time (or `fail`). comes first, and that anytime you use `for` it should be between the time and the person's name.

In other words, the most complex call you can make would look more or less like this:  
`/mini 100 for Brad +1 -ow`

# Printing Tables
You can print the spreadsheet tables using the `/mini` slash command too. All the tables are printed privately (meaning only you can see them), so you don't need to worry about flooding a channel if you want to check out some _rankings_.

You can check out today's _ore_ (ahem, medals) with `/mini ore`

You can print the Daily Mini dashboard by calling `/mini dash`,  `/mini dashboard`, or `/mini scoreboard`. Some of the award names have been changed slightly from the spreadsheet so they fit on narrow mobile devices.

You can print any Ranking table by using the keywords `rank`, `ranks`, or `rankings` followed by an abbreviation for the table.  
For example `/mini ranks w` will print the all-time wins table.

The tables and abbreviations (case insensitive) are:
- Average Times: `AVERAGES`, `AVG`, or `A`
- Personal Bests: `BESTS`, `BEST`, or `B`
- Games Played: `GAMES`, `GAME`, or `G`
- Monthly Leaderboard: `MONTH`, `MO`, or `M`
- All-Time Points: `POINTS`, `POINT`, or `P`
- All-Time Wins: `WINS`, `WIN`, or `W`

# Privacy
Privacy is important and I tried to take it seriously for this project, so it will probably be difficult to use this package out-of-the-box. There are three files that live on the secure server: `key.txt`, `token.txt`, and `unames.R`. These contain the key to the spreadsheet, the Slack team token, and player usernames, respectively. As far as I know, there's no way to access any of this information though the API.

# Help and reminders
You can get help anytime by typing `/mini help`

You can get a link to the Google spreadsheet by typing `/mini link`
