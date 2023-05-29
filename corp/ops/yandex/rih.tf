# Deployment configuration for russiaishiring.com
#
# The frontend of the page is served from a storage bucket, the
# backend runs in a container.

resource "yandex_dns_zone" "russiaishiring_com" {
  name      = "russiaishiring-com"
  zone      = "russiaishiring.com."
  public    = true
  folder_id = local.rih_folder_id
}

resource "yandex_iam_service_account" "rih_storage_sa" {
  name      = "rih-storage-sa"
  folder_id = local.rih_folder_id
}

resource "yandex_resourcemanager_folder_iam_member" "rih_sa_storage_editor" {
  folder_id = local.rih_folder_id
  role      = "storage.editor"
  member    = "serviceAccount:${yandex_iam_service_account.rih_storage_sa.id}"
}

resource "yandex_iam_service_account_static_access_key" "rih_sa_static_key" {
  service_account_id = yandex_iam_service_account.rih_storage_sa.id
  description        = "RIH bucket access key"
}

resource "yandex_storage_bucket" "rih_storage_bucket" {
  access_key = yandex_iam_service_account_static_access_key.rih_sa_static_key.access_key
  secret_key = yandex_iam_service_account_static_access_key.rih_sa_static_key.secret_key
  bucket     = "russiaishiring.com"
  folder_id  = local.rih_folder_id
  acl        = "public-read"

  https {
    certificate_id = yandex_cm_certificate.russiaishiring_com.id
  }

  website {
    index_document = "index.html"
  }
}

resource "yandex_cm_certificate" "russiaishiring_com" {
  folder_id = local.rih_folder_id
  name      = "russiaishiring-com"
  domains   = ["russiaishiring.com"]

  managed {
    challenge_type = "DNS_CNAME"
  }
}

resource "yandex_dns_recordset" "acme_russiaishiring_com" {
  zone_id = yandex_dns_zone.russiaishiring_com.id
  name    = yandex_cm_certificate.russiaishiring_com.challenges[0].dns_name
  type    = yandex_cm_certificate.russiaishiring_com.challenges[0].dns_type
  data    = [yandex_cm_certificate.russiaishiring_com.challenges[0].dns_value]
  ttl     = 60
}

resource "yandex_dns_recordset" "aname_russiaishiring_com" {
  zone_id = yandex_dns_zone.russiaishiring_com.id
  name    = "russiaishiring.com."
  type    = "ANAME"
  data    = ["russiaishiring.com.website.yandexcloud.net"]
  ttl     = 600
}
