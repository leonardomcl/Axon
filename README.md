# Axon
A powerful client-server application that enables seamless file sharing and real-time messaging in a single platform.

## ✨ Features

### 📤 File Transfer
- Upload & Download files of any type and size
- Drag & Drop support for easy file sharing
- Progress indicators for file transfers
- Multiple file simultaneous transfers

### 💬 Real-time Chat
- Instant messaging between connected clients and server
- Message history persistence

### 🔒 Security
- Secure connections (SSL/TLS support)
- User authentication system

## INSTALATION/USAGE

- 1) Copy DLL FILES to executable path
- 2) Generate server.key and server.crt ```openssl req -new -newkey rsa:2048 -days 3650 -nodes -x509 -keyout server.key -out server.crt -subj "/C=BR/ST=Sao Paulo/L=Sao Paulo/O=My Company/OU=IT/CN=localhost"```
- 3) Copy server.key and server.crt to server executable path
