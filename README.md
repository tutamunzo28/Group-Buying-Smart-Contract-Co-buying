# Group Buying Smart Contract (Co-buying)
A decentralized group buying platform built on Stacks blockchain that enables users to pool funds together to unlock bulk discounts with automatic refunds if minimum requirements aren't met.

## 🌟 Features

- 🎯 **Campaign Creation**: Create group buying campaigns with customizable parameters
- 💰 **Bulk Discounts**: Automatic discount pricing when target goals are met
- 🔒 **Secure Participation**: One participation per user with input validation
- ⏰ **Time-based Campaigns**: Automatic expiration and deadline management
- 💸 **Auto Refunds**: Automatic refund system if minimum requirements not met
- 📊 **Real-time Stats**: Campaign progress tracking and analytics
- 🛡️ **Emergency Controls**: Admin emergency refund capabilities
- ⏳ **Campaign Extensions**: Creators can extend campaign duration

## 🚀 Quick Start

### Prerequisites
- Clarinet CLI installed
- Node.js for testing

### Installation

```bash
git clone <repository-url>
cd group-buying-contract
```

### Running Tests

```bash
clarinet test
```

### Deploying Contract

```bash
clarinet deploy
```

## 🔧 Contract Functions

### Public Functions

| Function | Description | Parameters |
|----------|-------------|------------|
| `create-campaign` | Create new group buying campaign | title, target-amount, min-participants, price-per-unit, discount-price, duration-blocks |
| `participate` | Join existing campaign | campaign-id, units |
| `finalize-campaign` | Finalize campaign (success/refund) | campaign-id |
| `claim-refund` | Claim refund for failed campaigns | campaign-id |
| `emergency-refund` | Admin emergency refund | campaign-id |
| `extend-campaign` | Extend campaign duration | campaign-id, additional-blocks |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-campaign` | Get campaign details |
| `get-participation` | Get user participation info |
| `get-campaign-stats` | Get campaign statistics |
| `is-campaign-successful` | Check if campaign met goals |
| `calculate-discount-savings` | Calculate potential savings |

## 🛡️ Security Features

- ✅ **Input Validation**: All parameters validated before execution
- 🔐 **Access Control**: Role-based permissions for sensitive operations
- 💎 **Reentrancy Protection**: Safe fund transfers with proper checks
- 🚫 **Double Participation Prevention**: Users can only participate once per campaign
- ⏰ **Time Validation**: Proper deadline and expiration checks
- 💰 **Fund Safety**: Automatic escrow and refund mechanisms

## 🎯 Optimizations

- 📈 **Gas Efficient**: Optimized data structures and minimal storage operations
- 🔄 **Batch Operations**: Efficient campaign finalization process
- 📊 **Smart Calculations**: On-demand statistics calculation
- 💾 **Storage Optimization**: Compact data mapping structures

## 🧪 Test Coverage

- ✅ Campaign creation with valid/invalid parameters
- ✅ User participation and duplicate prevention
- ✅ Campaign finalization (success/failure scenarios)
- ✅ Refund mechanisms and edge cases
- ✅ Access control and authorization
- ✅ Time-based functionality and expiration
- ✅ Emergency scenarios and admin controls

## 🖼️ Suggested UI Features

### 📊 Campaign Dashboard
- Real-time progress bars showing funding status
- Participant counter and time remaining
- Savings calculator showing potential discounts
- Campaign history and user participation tracking

### 📝 Campaign Creation Form
- Step-by-step campaign setup wizard
- Parameter validation with helpful tooltips
- Preview mode showing campaign details
- Bulk discount calculator

### 💰 Refund Center
- Automated refund claim interface
- Transaction history and status tracking
- Notification system for campaign updates
- One-click refund processing

### 📱 Mobile-Responsive Design
- Touch-friendly participation buttons
- Swipe navigation for campaign browsing
- Push notifications for campaign updates
- QR code sharing for campaigns

## 🔄 Usage Examples

### Creating a Campaign
```clarity
(contract-call? .group-buying create-campaign 
  "Bulk Coffee Beans" 
  u1000000 
  u10 
  u50000 
  u35000 
  u1440)
```

### Participating in Campaign
```clarity
(contract-call? .group-buying participate u1 u5)
```

### Checking Campaign Status
```clarity
(contract-call? .group-buying get-campaign-stats u1)
```

## 🐛 Bug Fixes & Improvements

- 🔧 Fixed block height references (updated to stacks-block-height)
- 🛡️ Enhanced input validation for all parameters
- 💰 Improved refund mechanism with proper error handling
- ⏰ Better time validation and expiration logic
- 📊 Optimized statistics calculation functions

## 🤝 Contributing

1. Fork the repository
2. Create feature branch
3. Add comprehensive tests
4. Submit pull request

## 📜 License

MIT License - see LICENSE file for details

---

Made with ❤️ for the Stacks ecosystem
```
