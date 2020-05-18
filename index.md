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

The 'getLoadings' function allows you to predict annual loadings of water pathogens from the onsite sanitation system technologies used in a given region of the world. 

| Pathogen Loadings               | Description          |
|:--------------------------|:---------------------------|
| Excreted                       | Number of pathogens excreted per year   |
| To groundwater             | Number of pathogens emitted to groundwater each year   |
| To the surface                 | Number of pathogens emitted to the surface each year      |
| Retained in subsurface    | Number of pathogens retained in the subsurface each year |
| Decayed/inactivated        | Number of pathogens inactivated per year | 
| Conveyed in fecal sludge to treatment   | Number of pathogens conveyed each year to treatment in fecal sludge |
| Conveyed in sewerage to treatment   | Number of pathogens conveyed each year to treatment in sewerage | 

## Example

The following shows the use of the getLoadings function for data from Kampala, Uganda, grouping the data by 'region' and showing results for the 'Virus' group.

``` r
getLoadings(onsiteData,by="region",group="Virus")

#>     region     excreted to_groundwater   to_surface retained_in_soil      decayed In_Fecal_Sludge    In_Sewage  stillViable Onsite_LRV Onsite_PR
#> 1  Central 3.470412e+16   7.208736e+14 1.241347e+15     6.487862e+15 1.696317e+16    4.204900e+13 9.142680e+15 1.114695e+16       0.49    0.6788
#> 2  Kawempe 1.828207e+17   2.923315e+15 3.819070e+15     2.630984e+16 1.427235e+17    1.714675e+14 6.738060e+15 1.365191e+16       1.13    0.9253
#> 3 Makindye 2.115370e+17   5.784264e+15 7.404292e+15     5.205838e+16 1.396717e+17    1.877473e+14 6.042660e+15 1.941896e+16       1.04    0.9082
#> 4   Nakawa 1.740476e+17   4.848475e+15 5.834954e+15     4.363628e+16 1.023125e+17    1.109438e+14 1.716174e+16 2.795611e+16       0.79    0.8394
#> 5   Rubaga 2.103805e+17   4.278101e+15 5.257688e+15     3.850291e+16 1.602164e+17    2.221318e+14 9.808800e+14 1.073880e+16       1.29    0.9490
```

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

![Branching](https://guides.github.com/activities/hello-world/branching.png)


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
