# DNS configuration for tvl.su

resource "glesys_dnsdomain" "tvl_su" {
  name = "tvl.su"
}

resource "glesys_dnsdomain_record" "tvl_su_NS1" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "NS"
  data   = "ns1.namesystem.se."
}

resource "glesys_dnsdomain_record" "tvl_su_NS2" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "NS"
  data   = "ns2.namesystem.se."
}

resource "glesys_dnsdomain_record" "tvl_su_NS3" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "NS"
  data   = "ns3.namesystem.se."
}

resource "glesys_dnsdomain_record" "tvl_su_apex_A" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "A"
  data   = var.whitby_ipv4
}

resource "glesys_dnsdomain_record" "tvl_su_apex_AAAA" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "AAAA"
  data   = var.whitby_ipv6
}

resource "glesys_dnsdomain_record" "tvl_su_whitby_A" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "whitby"
  type   = "A"
  data   = var.whitby_ipv4
}

resource "glesys_dnsdomain_record" "tvl_su_whitby_AAAA" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "whitby"
  type   = "AAAA"
  data   = var.whitby_ipv6
}

# This record is responsible for hosting ~all TVL services. Be
# mindful!
resource "glesys_dnsdomain_record" "tvl_su_wildcard" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "*"
  type   = "CNAME"
  data   = "whitby.tvl.su."
}

# # Google Domains mail forwarding configuration (no sending)
resource "glesys_dnsdomain_record" "tvl_su_MX_aspmx" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "MX"
  data   = "1 aspmx.l.google.com."
}

resource "glesys_dnsdomain_record" "tvl_su_MX_alt1" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "MX"
  data   = "5 alt1.aspmx.l.google.com."
}

resource "glesys_dnsdomain_record" "tvl_su_MX_alt2" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "MX"
  data   = "5 alt2.aspmx.l.google.com."
}

resource "glesys_dnsdomain_record" "tvl_su_MX_alt3" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "MX"
  data   = "10 alt3.aspmx.l.google.com."
}

resource "glesys_dnsdomain_record" "tvl_su_MX_alt4" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "MX"
  data   = "10 alt4.aspmx.l.google.com."
}

resource "glesys_dnsdomain_record" "tvl_su_TXT_google_site" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "TXT"
  data   = "google-site-verification=3ksTBzFK3lZlzD3ddBfpaHs9qasfAiYBmvbW2T_ejH4"
}

resource "glesys_dnsdomain_record" "tvl_su_TXT_google_spf" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "TXT"
  data   = "v=spf1 include:_spf.google.com ~all"
}

resource "glesys_dnsdomain_record" "tvl_su_TXT_google_dkim" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "google._domainkey"
  type   = "TXT"
  data   = "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAlqCbnGa8oPwrudJK60l6MJj3NBnwj8wAPXNGtYy2SXrOBi7FT+ySwW7ATpfv6Xq9zGDUWJsENPUlFmvDiUs7Qi4scnNvSO1L+sDseB9/q1m3gMFVnTuieDO/T+KKkg0+uYgMM7YX5PahsAAJJ+EMb/r4afl3tcBMPR64VveKQ0hiSHA4zIYPsB9FB+b8S5C46uyY0r6WR7IzGjq2Gzb1do0kxvaKItTITWLSImcUu5ZZuXOUKJb441frVBWur5lXaYuedkxb1IRTTK0V/mBODE1D7k73MxGrqlzaMPdCqz+c3hRE18WVUkBTYjANVXDrs3yzBBVxaIAeu++vkO6BvQIDAQAB"
}

# Yandex 360 setup

resource "glesys_dnsdomain_record" "tvl_su_TXT_yandex" {
  domain = glesys_dnsdomain.tvl_su.id
  host   = "@"
  type   = "TXT"
  data   = "yandex-verification: b99c43b7838949dc"
}
