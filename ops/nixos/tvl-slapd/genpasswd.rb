require 'securerandom'

passwd = SecureRandom.urlsafe_base64(15)

puts "your password: [[#{passwd}]]"
