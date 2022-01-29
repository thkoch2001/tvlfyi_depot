(ns bbbg.util.dev-secrets
  "Utility library for loading secrets during development from multiple
  backends.

  # Supported backends

  - [Pass][0] (the default)

        (bbbg.util.dev-secrets/set-backend! :pass)

    Loads all secrets by shelling out to `pass <secret-name>`

    [0]: https://www.passwordstore.org/

  - Directory

        (bbbg.util.dev-secrets/set-backend! [:dir \"/path/to/secret/directory\"])

     Loads all secrets by reading the secret name as a (plaintext!) file rooted
     at the given directory"
  (:require [bbbg.util.core :as u]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^:dynamic *secret-backend* :pass)

(defn set-backend!
  "Change the default secret-backend"
  [backend]
  (alter-var-root #'*secret-backend* (constantly backend)))

(defmulti ^:private load-secret
  (fn [backend _secret]
    (if (coll? backend) (first backend) backend)))

(defmethod load-secret :pass [_ secret]
  (u/pass secret))

(defmethod load-secret :dir [[_ dir] secret]
  (str/trim (slurp (io/file dir secret))))

(defn secret
  "Load the value for the given `secret-name' from the currently selected
  backend"
  [secret-name]
  (load-secret *secret-backend* secret-name))

(comment
  (secret "bbbg/discord-client-id")

  (binding [*secret-backend* [:dir "/tmp/bbbg-secrets"]]
    (secret "bbbg/discord-client-id"))

  (set-backend! [:dir "/tmp/bbbg-secrets"])
  (secret "bbbg/discord-client-id")

  (set-backend! :pass)
  (secret "bbbg/discord-client-id")
  )
