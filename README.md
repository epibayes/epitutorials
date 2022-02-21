# EPITUTORIALS
*Created using [Flexdashboard](https://rstudio.github.io/flexdashboard/articles/flexdashboard.html)*

## REPO CONTENTS
- `index.Rmd` - main file that builds the dashbaord. Running this document generates `index.html`
- `favicon.png` - the site favicon
- `logo.png` - the navbar brand
- `styles.css` - any external css

## ADDING CONTENT
### Adding a new, free-standing page
- Introduce a new page by using the following structure:
```
Page Title {data-orientation=rows} 
===================================== 
Row
-----------------------------------------------------------------------
### Content Title

Contents
```
- See full documentation on [Flexdashboard's Documentation Page](https://pkgs.rstudio.com/flexdashboard/articles/layouts.html#multiple-pages)

### Adding a new page within a menu item
- Add a page under a menu item by using the following structure:
```
Page Title {data-orientation=rows data-navmenu="Menu Item"}
=====================================
Row
-----------------------------------------------------------------------
### Content Title

Contents
```
- See full documentation on [Flexdashboard's Documentation Page](https://rstudio.github.io/flexdashboard/articles/using.html#page-navigation)