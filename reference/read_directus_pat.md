# Import Directus PAT This imports a Directus Personal Access Token from a file created by generate_directus_token. It also tests whether the token is valid.

Import Directus PAT This imports a Directus Personal Access Token from a
file created by generate_directus_token. It also tests whether the token
is valid.

## Usage

``` r
read_directus_pat(filepath = "directus_PAT.txt")
```

## Arguments

- filepath:

  A path to the file created by generate_directus_token().

## Value

returns a character string that can be used in the function
set_default_token().

## Examples

``` r
# not run:
# ## run in command line or read password from a non-indexed location:
# generate_directus_token(savedir = "afolder", savename = "directus.PAT.txt")
# mytoken <- read_directus_pat(file.path("afolder","directus.PAT.txt"))
# set_default_token(mytoken)
```
