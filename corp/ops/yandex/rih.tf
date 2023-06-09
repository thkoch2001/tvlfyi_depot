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
  role      = "storage.admin"
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

resource "yandex_container_registry" "rih_registry" {
  name      = "rih-registry"
  folder_id = local.rih_folder_id
}

resource "yandex_iam_service_account" "rih_backend" {
  name      = "rih-backend"
  folder_id = local.rih_folder_id
}

resource "yandex_resourcemanager_folder_iam_member" "rih_backend_image_pull" {
  folder_id = local.rih_folder_id
  role      = "container-registry.images.puller"
  member    = "serviceAccount:${yandex_iam_service_account.rih_backend.id}"
}

resource "yandex_serverless_container" "rih_backend" {
  name               = "rih-backend"
  folder_id          = local.rih_folder_id
  memory             = 128
  execution_timeout  = "10s"
  cores              = 1
  core_fraction      = 100
  service_account_id = yandex_iam_service_account.rih_backend.id

  image {
    url = "cr.yandex/crpkcq65tn6bhq6puq2o/rih-backend:9cwnx8jvwjw2ckpqg970p4y7cf74z28j"
  }

  secrets {
    id                   = yandex_lockbox_secret.rih_backend_storage_key.id
    version_id           = yandex_lockbox_secret_version.rih_backend_storage_secret.id
    key                  = "access_key"
    environment_variable = "AWS_ACCESS_KEY_ID"
  }

  secrets {
    id                   = yandex_lockbox_secret.rih_backend_storage_key.id
    version_id           = yandex_lockbox_secret_version.rih_backend_storage_secret.id
    key                  = "secret_key"
    environment_variable = "AWS_SECRET_ACCESS_KEY"
  }
}

resource "yandex_api_gateway" "rih_gateway" {
  name      = "rih-gateway"
  folder_id = local.rih_folder_id

  custom_domains {
    fqdn           = "api.russiaishiring.com"
    certificate_id = yandex_cm_certificate.api_russiaishiring_com.id
  }

  depends_on = [
    yandex_cm_certificate.api_russiaishiring_com,
    yandex_dns_recordset.acme_api_russiaishiring_com,
  ]

  spec = <<-EOT
    openapi: "3.0.0"
    info:
      version: 1.0.0
      title: RIH API
    paths:
      /{proxy+}:
        x-yc-apigateway-any-method:
          x-yc-apigateway-integration:
            type: serverless_containers
            container_id: ${yandex_serverless_container.rih_backend.id}
            service_account_id: ${yandex_iam_service_account.rih_backend.id}
          parameters:
          - explode: false
            in: path
            name: proxy
            required: false
            schema:
              default: '-'
              type: string
            style: simple
  EOT
}

resource "yandex_cm_certificate" "api_russiaishiring_com" {
  folder_id = local.rih_folder_id
  name      = "api-russiaishiring-com"
  domains   = ["api.russiaishiring.com"]

  managed {
    challenge_type = "DNS_CNAME"
  }
}

resource "yandex_dns_recordset" "acme_api_russiaishiring_com" {
  zone_id = yandex_dns_zone.russiaishiring_com.id
  name    = yandex_cm_certificate.api_russiaishiring_com.challenges[0].dns_name
  type    = yandex_cm_certificate.api_russiaishiring_com.challenges[0].dns_type
  data    = [yandex_cm_certificate.api_russiaishiring_com.challenges[0].dns_value]
  ttl     = 60
}

resource "yandex_dns_recordset" "cname_api_russiaishiring_com" {
  zone_id = yandex_dns_zone.russiaishiring_com.id
  name    = "api.russiaishiring.com."
  type    = "CNAME"
  data    = [yandex_api_gateway.rih_gateway.domain]
  ttl     = 600
}

# Bucket setup for data receival bucket
#
# The bucket is set up and controlled by the default storage account,
# but a separate key is set up for the rih-backend IAM account which
# can only access the information in this bucket.

resource "yandex_kms_symmetric_key" "backend_data_key" {
  name              = "rih-backend-data-key"
  default_algorithm = "AES_128"
  rotation_period   = "4380h" # ~6 months

  lifecycle {
    prevent_destroy = true
  }
}

resource "yandex_storage_bucket" "rih_backend_data" {
  access_key = yandex_iam_service_account_static_access_key.rih_sa_static_key.access_key
  secret_key = yandex_iam_service_account_static_access_key.rih_sa_static_key.secret_key
  bucket     = "rih-backend-data"
  folder_id  = local.rih_folder_id
  acl        = "private"

  versioning {
    enabled = true
  }

  server_side_encryption_configuration {
    rule {
      apply_server_side_encryption_by_default {
        kms_master_key_id = yandex_kms_symmetric_key.backend_data_key.id
        sse_algorithm     = "aws:kms"
      }
    }
  }

  lifecycle {
    prevent_destroy = true
  }
}

resource "yandex_iam_service_account_static_access_key" "rih_backend_static_key" {
  service_account_id = yandex_iam_service_account.rih_backend.id
  description        = "RIH backend bucket access key"
}

resource "yandex_lockbox_secret" "rih_backend_storage_key" {
  name      = "rih-backend-storage-key"
  folder_id = local.rih_folder_id
}

resource "yandex_lockbox_secret_version" "rih_backend_storage_secret" {
  secret_id = yandex_lockbox_secret.rih_backend_storage_key.id

  entries {
    key        = "access_key"
    text_value = yandex_iam_service_account_static_access_key.rih_backend_static_key.access_key
  }

  entries {
    key        = "secret_key"
    text_value = yandex_iam_service_account_static_access_key.rih_backend_static_key.secret_key
  }
}

# TODO(tazjin): needs provider update
#
# resource "yandex_lockbox_secret_iam_binding" "viewer" {
#   secret_id = yandex_lockbox_secret.rih_backend_storage_key.id
#   role = "viewer"

#   members = [
#     "serviceAccount:${yandex_iam_service_account.rih_backend.id}"
#   ]
# }
