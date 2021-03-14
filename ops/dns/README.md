DNS configuration
=================

This folder contains configuration for our DNS zones. The zones are hosted with
Google Cloud DNS, which supports zone-file based import/export.

Currently there is no automation to deploy these zones, but CI will check their
integrity.

*Note: While each zone file specifies an SOA record, it only exists to satisfy
`named-checkzone`. Cloud DNS manages this record for us.*
