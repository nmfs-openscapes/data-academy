# NMFS Data Academy

Notes and scripts for administering the [NMFS Data Academy](https://nmfs-openscapes.github.io/data-academy) on dataquest.io.

## Using the Dataquest API

We are using the [dataquest.io API](https://support.dataquest.io/en/articles/680-teams-api-documentation) for processing data on participants's progress. All API calls require our Team ID and API Key. Our Team ID is documented in our [planning document](https://docs.google.com/document/d/1S_XuI-HGrckq8ySwFvmdQAmBdfMztqBIQ9K5eyfooDs/edit?usp=sharing), and the API Key is created on the [settings page](https://app.dataquest.io/team/settings) under Team Management on dataquest.io.

Store both of these values in your `.Renviron` file. The easiest way to find and open this file for editing is with the [edit_r_environ()](https://usethis.r-lib.org/reference/edit.html) function in the [usethis package](https://usethis.r-lib.org/):

```r
usethis::edit_r_environ()
```

Then in that file record the team ID and API key as:

```
DATAQUEST_TEAM_ID=our-team-id
DATAQUEST_API_KEY=our-api-key
```

The scripts will read those keys as needed when querying the API.
