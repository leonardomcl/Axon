# Axon
A powerful client-server application that enables seamless file sharing and real-time messaging in a single platform.

https://github.com/user-attachments/assets/75fe35c5-b171-4478-b419-12ff1882abca

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
- ban system after authentication errors

## INSTALATION/USAGE

- 1) Copy DLL FILES to **client** AND **server** executable path
- 2) Generate _server.key_ and _server.crt_ ```openssl req -new -newkey rsa:2048 -days 3650 -nodes -x509 -keyout server.key -out server.crt -subj "/C=BR/ST=Sao Paulo/L=Sao Paulo/O=My Company/OU=IT/CN=localhost"```
- 3) Copy _server.key_ and _server.crt_ to **server** executable path

## DEPENDENCIES

[Indy Sockets](https://github.com/IndySockets/Indy)
[TaurusTLS](https://github.com/JPeterMugaas/TaurusTLS)
