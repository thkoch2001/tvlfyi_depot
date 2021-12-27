# DNS configuration for nixery.dev
#
# TODO(tazjin): Figure out what to do with //ops/dns for this. I'd
# like to keep zonefiles in case we move providers again, but maybe
# generate something from them. Not sure yet.

resource "glesys_dnsdomain" "nixery_dev" {
  name = "nixery.dev"
}

resource "glesys_dnsdomain_record" "nixery_dev_apex_A" {
  domain = glesys_dnsdomain.nixery_dev.id
  host   = "@"
  type   = "A"
  data   = "49.12.129.211"
}

resource "glesys_dnsdomain_record" "nixery_dev_apex_AAAA" {
  domain = glesys_dnsdomain.nixery_dev.id
  host   = "@"
  type   = "AAAA"
  data   = "2a01:4f8:242:5b21:0:feed:edef:beef"
}

resource "glesys_dnsdomain_record" "nixery_dev_NS1" {
  domain = glesys_dnsdomain.nixery_dev.id
  host   = "@"
  type   = "NS"
  data   = "ns1.namesystem.se."
}

resource "glesys_dnsdomain_record" "nixery_dev_NS2" {
  domain = glesys_dnsdomain.nixery_dev.id
  host   = "@"
  type   = "NS"
  data   = "ns2.namesystem.se."
}

resource "glesys_dnsdomain_record" "nixery_dev_NS3" {
  domain = glesys_dnsdomain.nixery_dev.id
  host   = "@"
  type   = "NS"
  data   = "ns3.namesystem.se."
}
