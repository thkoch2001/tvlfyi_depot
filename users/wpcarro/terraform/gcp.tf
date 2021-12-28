provider "google" {
  project = "wpcarros-infrastructure"
  region = "us-central1"
  zone = "us-central1-a"
}

data "google_compute_default_service_account" "default" {}

resource "google_compute_instance" "default" {
  name = "diogenes-2"
  machine_type = "e2-standard-2"
  zone = "us-central1-a"
  hostname = "diogenes.wpcarro.dev"

  tags = [
    "http-server",
    "https-server",
    "mosh-server",
    "quassel-core",
  ]

  boot_disk {
    device_name = "boot"

    initialize_params {
      size = 10
      image = "nixos-20-03"
    }
  }

  network_interface {
    network = "default"
    subnetwork = "default"

    access_config {
      public_ptr_domain_name = "wpcarro.dev"
    }
  }

  metadata = {
    enable-oslogin = "TRUE"
  }

  service_account {
    scopes = ["cloud-platform"]
  }
}