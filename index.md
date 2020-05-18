---
layout: default
---

# pathogenflows

**pathogenflows** is an R package providing functions that can be used to predict pathogen flows throughout a sanitation system, based on the types of technologies used.

## Installation

The latest version of pathogenflows can be installed from github (if you don't have the package **devtools**, you will need to install it first):

``` r
# install.packages("devtools")
devtools::install_github('mverbyla/pathogenflows')
```

## Example use of pathogenflows

The **getLoadings** function allows you to predict annual loadings of water pathogens from the onsite sanitation system technologies used in a given region of the world. 

![Pathogen Flows](https://raw.githubusercontent.com/mverbyla/pathogenflows/master/images/pft_flow_chart.png)

| Pathogen Loadings               | Description          |
|:--------------------------|:---------------------------|
| Excreted                       | Number of pathogens excreted per year   |
| To groundwater             | good and plenty   |
| To the surface                 | good `oreos`      |
| Retained in subsurface             | good `zoute` drop |
| Decayed/inactivated            | good `zoute` drop | 
| Conveyed in fecal sludge to treatment   | good `zoute` drop |
| Conveyed in sewerage to treatment   | good `zoute` drop | 

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
