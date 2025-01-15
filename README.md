# About this repository

This is a repo to host various interactive graphics that UBC is using to help assess the impacts of WVS Injunction Measures.
For full descriptions of the WVS measures, see the [WV Project Injunction webpage](https://www.nwp.usace.army.mil/Locations/Willamette-Valley/Injunction/).

Interactive graphics created by UBC include: 

* An timeline of project-by-project implementation measures, arranged by sub-basin. The timeline shows when different measures were implemented at which times, and shows instances where the measures had to be bypassed due to, e.g., flood risk mitigation. 
* An interactive map of rotary screw trap (RST) locations, showing the locations of the traps at each project of interest. By hovering over the map, you can see years in which each trap was active during pre- and post-injunction periods. 

View the webpage by clicking on this link: [https://ubc-willamette-lcm-team.github.io/WVSInjunctionAnalysis.io/](https://ubc-willamette-lcm-team.github.io/WVSInjunctionAnalysis.io/)

## Folder structure and making updates to the website

This webpage is built from a series of RMarkdown files that are compiled into html files by `render.R`: the three files are `index.Rmd` (the landing page of the website), `timesline_static.Rmd`, `rst_map.Rmd`, and `rst_onoff_plots.Rmd`.
The markdown files pull datasets from the Teams "Injunction Analysis" folder to build various visualizations including tables, maps, and graphics of on/off times for rotary screw traps.

To modify the webpage, you can edit existing R Markdown files, or add new ones to be built into new webpages.
After creating a new .Rmd file, edit `_site.yml` to add the new webpage and its title. 
Then, run `render.R` to build the html files (compiled html files are placed in the docs/ folder).
Finally, push the changes to the GitHub repository, and after a few minutes the webpage should reflect the updates.
