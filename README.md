# 🏠 Propchain - Property Co-Ownership Registry

> 🚀 **Fractional real estate ownership made simple on the Stacks blockchain**

## 📋 Overview

Propchain is a smart contract that enables **fractional ownership** of real estate properties. Users can create properties, divide them into shares, and allow others to buy, sell, and transfer ownership stakes seamlessly.

## ✨ Features

- 🏘️ **Create Properties**: Register new properties with customizable share structures
- 💰 **Buy Shares**: Purchase fractional ownership in any listed property
- 🔄 **Transfer Shares**: Send ownership stakes to other users
- 📊 **Portfolio Tracking**: View all your property investments in one place
- 💎 **Dynamic Pricing**: Property owners can adjust share prices
- 📈 **Ownership Analytics**: Calculate ownership percentages and portfolio values

## 🛠️ Core Functions

### Public Functions

#### `create-property`
```clarity
(create-property name description total-shares share-price)
```
- Creates a new property listing
- **Parameters**: Property name, description, total shares, price per share
- **Returns**: Property ID

#### `buy-shares`
```clarity
(buy-shares property-id shares)
```
- Purchase shares of an existing property
- **Parameters**: Property ID, number of shares to buy
- **Returns**: Number of shares purchased

#### `transfer-shares`
```clarity
(transfer-shares property-id recipient shares)
```
- Transfer your shares to another user
- **Parameters**: Property ID, recipient address, number of shares
- **Returns**: Number of shares transferred

#### `update-share-price`
```clarity
(update-share-price property-id new-price)
```
- Update the price per share (property owner only)
- **Parameters**: Property ID, new price per share
- **Returns**: New share price

### Read-Only Functions

#### `get-property`
```clarity
(get-property property-id)
```
- Get detailed information about a property

#### `get-user-shares`
```clarity
(get-user-shares property-id user)
```
- Get number of shares a user owns in a property

#### `get-user-properties`
```clarity
(get-user-properties user)
```
- Get list of all properties a user has invested in

#### `calculate-ownership-percentage`
```clarity
(calculate-ownership-percentage property-id user)
```
- Calculate user's ownership percentage in a property

#### `get-user-portfolio-value`
```clarity
(get-user-portfolio-value user)
```
- Calculate total value of user's property portfolio

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. **Clone the repository**
```bash
git clone <your-repo-url>
cd propchain
```

2. **Check the contract**
```bash
clarinet check
```

3. **Run tests**
```bash
clarinet test
```

4. **Deploy locally**
```bash
clarinet integrate
```

## 💡 Usage Examples

### Creating a Property
```clarity
(contract-call? .propchain create-property 
  "Sunset Villa" 
  "Beautiful 3BR house in downtown" 
  u1000 
  u50)
```

### Buying Shares
```clarity
(contract-call? .propchain buy-shares u1 u100)
```

### Checking Your Portfolio
```clarity
(contract-call? .propchain get-user-properties tx-sender)
(contract-call? .propchain get-user-portfolio-value tx-sender)
```

## 🔒 Security Features

- ✅ Owner-only functions for property management
- ✅ Input validation for all parameters
- ✅ Safe arithmetic operations
- ✅ Proper error handling
- ✅ STX transfer validation

## 🎯 Use Cases

- 🏢 **Commercial Real Estate**: Split ownership of office buildings
- 🏠 **Residential Properties**: Co-own vacation homes or rental properties  
- 🏭 **Industrial Assets**: Fractional ownership of warehouses or factories
- 🌍 **International Investment**: Cross-border property investment made easy

## 📊 Contract Limits

- Maximum 100 shareholders per property
- Maximum 50 properties per user
- String limits: 100 chars (name), 500 chars (description)

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License.



