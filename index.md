---
layout: default
---

## Installation

The latest version of pathogenflows can be installed from github (if you don't have the package **devtools**, you will need to install it first):

``` r
# install.packages("devtools")
devtools::install_github('mverbyla/pathogenflows')
```

## About

The **getLoadings** function allows you to predict annual loadings of water pathogens from the onsite sanitation system technologies used in a given region of the world. 

![PathogenFlows](https://raw.githubusercontent.com/mverbyla/pathogenflows/master/images/pft_flow_chart.png)

| Pathogen Loadings               | Description          |
|:--------------------------|:---------------------------|
| Excreted                       | Number of pathogens excreted per year   |
| To groundwater             | Number of pathogens emitted to groundwater each year   |
| To the surface                 | Number of pathogens emitted to the surface each year      |
| Retained in subsurface    | Number of pathogens retained in the subsurface each year |
| Decayed/inactivated        | Number of pathogens inactivated per year | 
| Conveyed in fecal sludge to treatment   | Number of pathogens conveyed each year to treatment in fecal sludge |
| Conveyed in sewerage to treatment   | Number of pathogens conveyed each year to treatment in sewerage | 

The 'getLoadings' 


Text can be **bold**, _italic_, or ~~strikethrough~~.

[Link to another page](./another-page.html).

There should be whitespace between paragraphs.

There should be whitespace between paragraphs. We recommend including a README, or a file with information about your project.

# Header 1

This is a normal paragraph following a header. GitHub is a code hosting platform for version control and collaboration. It lets you and others work together on projects from anywhere.

## Header 2

> This is a blockquote following a header.
>
> When something is important enough, you do it even if the odds are not in your favor.

### Header 3

```js
// Javascript code with syntax highlighting.
var fun = function lang(l) {
  dateformat.i18n = require('./lang/' + l)
  return true;
}
```

```ruby
# Ruby code with syntax highlighting
GitHubPages::Dependencies.gems.each do |gem, version|
  s.add_dependency(gem, "= #{version}")
end
```

#### Header 4

*   This is an unordered list following a header.
*   This is an unordered list following a header.
*   This is an unordered list following a header.

##### Header 5

1.  This is an ordered list following a header.
2.  This is an ordered list following a header.
3.  This is an ordered list following a header.

###### Header 6

| head1        | head two          | three |
|:-------------|:------------------|:------|
| ok           | good swedish fish | nice  |
| out of stock | good and plenty   | nice  |
| ok           | good `oreos`      | hmm   |
| ok           | good `zoute` drop | yumm  |

### There's a horizontal rule below this.

* * *

### Here is an unordered list:

*   Item foo
*   Item bar
*   Item baz
*   Item zip

### And an ordered list:

1.  Item one
1.  Item two
1.  Item three
1.  Item four

### And a nested list:

- level 1 item
  - level 2 item
  - level 2 item
    - level 3 item
    - level 3 item
- level 1 item
  - level 2 item
  - level 2 item
  - level 2 item
- level 1 item
  - level 2 item
  - level 2 item
- level 1 item

### Small image

![Octocat](https://github.githubassets.com/images/icons/emoji/octocat.png)

### Large image

![Branching](https://raw.githubusercontent.com/mverbyla/pathogenflows/master/images/pft_flow_chart.png)


### Definition lists can be used with HTML syntax.

<dl>
<dt>Name</dt>
<dd>Godzilla</dd>
<dt>Born</dt>
<dd>1952</dd>
<dt>Birthplace</dt>
<dd>Japan</dd>
<dt>Color</dt>
<dd>Green</dd>
</dl>

```
Long, single-line code blocks should not wrap. They should horizontally scroll if they are too long. This line should be long enough to demonstrate this.
```

```
The final element.
```
