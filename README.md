# slackrsheets
A hyper-specific R package to facilitate posting data to a Google Spreadsheet from Slack

This package is for an extremely specific purpose: for taking 'slash' commands given in a slack client and posting the input to those    commands as cell values in a specific Google sheet. 

The package also contains functions for reading summary pages in that sheet and printing plain-text versions of those summary pages. 

Here's an example of one type of table:
```
 Rank | Player |  Average Time
-----  -------  -------------
   1.  Brian            00:48
   2.  Jackie           00:53
   3.  Katie            00:56
   4.  Chad             01:00
   5.  Brad             01:11
   6.  Colin            01:17
   7.  Matt             01:20
   8.  Eric             01:21
   9.  Phil             01:27
  10.  Maggie           01:32
  11.  Kyle             01:58
```
Finally, the package makes simple plots of the data contained in the spreadsheet (pending).
