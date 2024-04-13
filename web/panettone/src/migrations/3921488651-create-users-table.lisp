"Add a table to store information about users, load the initial set of users
from the authentication provider, and change fks for other tables"

(defun up ()
  (panettone.model:create-table-if-not-exists
   'panettone.model:user))
