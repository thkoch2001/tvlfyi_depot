provider "google" {
  project = "wpcarros-infrastructure"
  region  = "us-central1"
  zone    = "us-central1-a"
}

data "google_compute_default_service_account" "default" {}

resource "google_compute_instance" "default" {
  name         = "diogenes-2"
  machine_type = "e2-standard-2"
  zone         = "us-central1-a"
  hostname     = "diogenes.wpcarro.dev"

  tags = [
    "http-server",
    "https-server",
    "diogenes-firewall"
  ]

  boot_disk {
    device_name = "boot"

    initialize_params {
      size  = 10
      image = "projects/nixos-cloud/global/images/nixos-image-20-09-3531-3858fbc08e6-x86-64-linux"
    }
  }

  attached_disk {
    source      = "diogenes-2-disk"
    device_name = "diogenes-2-disk"
  }

  network_interface {
    network    = "default"
    subnetwork = "default"

    access_config {}
  }

  metadata = {
    # sshKeys is deprecated, but the GCE NixOS image relies on it, so we need
    # both values:
    # - deprecation: https://cloud.google.com/compute/docs/metadata/default-metadata-values
    # - NixOS bug: https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/fetch-instance-ssh-keys.bash#L14
    ssh-keys = "wpcarro:ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJkNQJBXekuSzZJ8+gxT+V1+eXTm3hYsfigllr/ARXkf wpcarro@gmail.com"
    sshKeys  = "wpcarro:ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJkNQJBXekuSzZJ8+gxT+V1+eXTm3hYsfigllr/ARXkf wpcarro@gmail.com"
  }

  service_account {
    scopes = ["cloud-platform"]
  }
}

resource "google_compute_firewall" "default" {
  name    = "diogenes-firewall"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["6698"]
  }

  allow {
    protocol = "udp"
    ports = [
      "60000-61000" # mosh
    ]
  }

  source_tags = ["diogenes-firewall"]
}

resource "google_compute_disk" "default" {
  name = "diogenes-2-disk"
  zone = "us-central1-a"
  size = 100
}