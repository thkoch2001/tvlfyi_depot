# Terraform configuration for TVL corp infrastructure (on Yandex
# Cloud).

terraform {
  required_providers {
    yandex = {
      source = "yandex-cloud/yandex"
    }
  }

  # Credentials need to be sourced from creds.fish
  backend "s3" {
    endpoint = "storage.yandexcloud.net"
    bucket   = "su-tvl-terraform-state"
    region   = "ru-central1"
    key      = "corp/ops/terraform.tfstate"

    skip_region_validation      = true
    skip_credentials_validation = true
  }
}

provider "yandex" {
  zone = "ru-central1-b"
}

locals {
  tvl_cloud_id  = "b1ggu5m1btue982app12"
  tvl_folder_id = "b1gmbeqt9o5kbl7rclln"
  rih_cloud_id  = "b1glccvcqggi2ruibgvt"
  rih_folder_id = "b1gsavcrsjn059d1sbh9"
}

# Storage state bucket configuration

resource "yandex_iam_service_account" "tf_state_sa" {
  folder_id = local.tvl_folder_id
  name      = "terraform-state"
}

resource "yandex_resourcemanager_folder_iam_member" "tf_state_sa_storage" {
  folder_id = local.tvl_folder_id
  role      = "storage.editor"
  member    = "serviceAccount:${yandex_iam_service_account.tf_state_sa.id}"
}

resource "yandex_iam_service_account_static_access_key" "tf_state_sa_key" {
  service_account_id = yandex_iam_service_account.tf_state_sa.id
  description        = "Static access key for Terraform state"
}

resource "yandex_storage_bucket" "tf_state" {
  access_key = yandex_iam_service_account_static_access_key.tf_state_sa_key.access_key
  secret_key = yandex_iam_service_account_static_access_key.tf_state_sa_key.secret_key
  bucket     = "su-tvl-terraform-state"
}

resource "yandex_dns_zone" "russiaishiring_com" {
  name      = "russiaishiring-com"
  zone      = "russiaishiring.com."
  public    = true
  folder_id = local.rih_folder_id
}

# Secret management configuration

resource "yandex_kms_symmetric_key" "tvl_credentials_key" {
  name              = "tvl-credentials"
  folder_id         = local.tvl_folder_id
  default_algorithm = "AES_256"
  rotation_period   = "2160h" # 90 days
}

resource "yandex_kms_secret_ciphertext" "tf_state_key" {
  key_id    = yandex_kms_symmetric_key.tvl_credentials_key.id
  plaintext = yandex_iam_service_account_static_access_key.tf_state_sa_key.secret_key
}
