# About this repository

This is a repo to host various interactive graphics that UBC is using to help assess the impacts of WVS Injunction Measures.
For full descriptions of the WVS measures, see the [WV Project Injunction webpage](https://www.nwp.usace.army.mil/Locations/Willamette-Valley/Injunction/).

Interactive graphics created by UBC include: 

* An timeline of project-by-project implementation measures, arranged by sub-basin. The timeline shows when different measures were implemented at which times, and shows instances where the measures had to be bypassed due to, e.g., flood risk mitigation. 
* An interactive map of rotary screw trap (RST) locations, showing the locations of the traps at each project of interest. By hovering over the map, you can see years in which each trap was active during pre- and post-injunction periods. 

View the webpage by clicking on this link: [https://ubc-willamette-lcm-team.github.io/WVSInjunctionAnalysis.io/](https://ubc-willamette-lcm-team.github.io/WVSInjunctionAnalysis.io/)

## Folder structure and making updates to the website

This webpage is built from a series of RMarkdown files that are compiled into html files by `render.R`. 
As of January 2025, there are four files: 

* `index.Rmd` (the landing page of the website),
* `timeline_static.Rmd`, which hosts interactive timelines of when injunction measures were implemented at each site, 
* `rst_map.Rmd`, which hosts an interactive map graphic showing the locations of rotary screw traps (RSTs) and USGS gages, 
* `rst_onoff_plots.Rmd`, which hosts an interactive graphic showing the on, off, and operational dates of RSTs located at sites in the Willamette basin. 

The above markdown files pull datasets created by code hosted on Teams (see "Injunction Analysis > InjunctionRSTDashboardWebsite" folder and its associated README file for information about the code and data used to generate the graphics hosted on this website).

## Making changes to the website

To modify the webpage, you can edit the existing R Markdown files or add new ones that will be rendered into .html files and hosted on the website. 
After editing or creating a new .Rmd file, run `render.R` to build the html files (compiled html files are placed in the docs/ folder).

If you made a new file, you have have to update the `_site.yml` file so that the website knows to host the .html file as a new tab.
Here, specify the title of the webpage tab and the name of the newly rendered html file (you do not have to specify the `/docs/` path, only the file name).
For example, the following snippet from `_site.yml` indicates that the timeline_static.html file should be hosted under the title "Injunction timeline".

```
navbar:
  title: "WVS Injunction Assessment"
  left:
    - text: "Injunction timeline"
      href: timeline_static.html
    ...
```

Once you have edited the R Markdown file, rendered it to html, and optionally updated the `_site.yml` file, push the changes to the GitHub repository.
After a few minutes the webpage should update to reflect the updates.

See the documentation for the `rmarkdown` R library, particularly the `render_site` function, for more details.