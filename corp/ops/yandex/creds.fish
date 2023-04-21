export YC_TOKEN=(yc iam create-token)
export YC_CLOUD_ID=(yc config get cloud-id)
export YC_FOLDER_ID=(yc config get folder-id)
export AWS_ACCESS_KEY_ID="YCAJE6eRLY8Az-9kveNRtz4sh"
export AWS_SECRET_ACCESS_KEY=(yc kms symmetric-crypto decrypt --name tvl-credentials --cloud-id b1ggu5m1btue982app12 --folder-name default --ciphertext-file encrypted-state-secret.key --plaintext-file /dev/stdout | head -n1)
