"""
bank_account.py - A simple banking system implementation
This module provides classes for managing bank accounts with basic operations.
"""

import random
import datetime
from typing import List, Optional, Dict, Union

class BankAccount:
    """A class representing a basic bank account"""
    
    account_counter = 1000  # Class variable to generate unique account numbers
    
    def __init__(self, account_holder: str, initial_balance: float = 0.0):
        self.account_number = BankAccount.account_counter
        BankAccount.account_counter += 1
        self.account_holder = account_holder
        self.balance = initial_balance
        self.transaction_history = []
        self.is_active = True
        self.overdraft_limit = 100.0  # Default overdraft limit
        self._add_transaction("Account opened", initial_balance)
    
    def _add_transaction(self, description: str, amount: float):
        """Private method to record transactions"""
        transaction = {
            "date": datetime.datetime.now(),
            "description": description,
            "amount": amount,
            "balance_after": self.balance
        }
        self.transaction_history.append(transaction)
    
    def deposit(self, amount: float) -> bool:
        """Deposit money into the account"""
        if amount <= 0:
            print("Deposit amount must be positive.")
            return False
        
        self.balance += amount
        self._add_transaction("Deposit", amount)
        print(f"Deposited ${amount:.2f}. New balance: ${self.balance:.2f}")
        return True
    
    def withdraw(self, amount: float) -> bool:
        """Withdraw money from the account"""
        if amount <= 0:
            print("Withdrawal amount must be positive.")
            return False
        
        if not self.is_active:
            print("Account is inactive. Cannot withdraw.")
            return False
        
        available_balance = self.balance + self.overdraft_limit
        if amount > available_balance:
            print(f"Insufficient funds. Available: ${available_balance:.2f}")
            return False
        
        self.balance -= amount
        self._add_transaction("Withdrawal", -amount)
        print(f"Withdrew ${amount:.2f}. New balance: ${self.balance:.2f}")
        return True
    
    def transfer(self, amount: float, recipient: 'BankAccount') -> bool:
        """Transfer money to another account"""
        if not self.is_active or not recipient.is_active:
            print("One or both accounts are inactive.")
            return False
        
        if self.withdraw(amount):
            recipient.deposit(amount)
            self._add_transaction(f"Transfer to {recipient.account_number}", -amount)
            recipient._add_transaction(f"Transfer from {self.account_number}", amount)
            print(f"Transferred ${amount:.2f} to account {recipient.account_number}")
            return True
        return False
    
    def get_statement(self, days: int = 30) -> List[Dict]:
        """Get recent transactions within specified days"""
        cutoff_date = datetime.datetime.now() - datetime.timedelta(days=days)
        recent_transactions = [
            t for t in self.transaction_history 
            if t["date"] >= cutoff_date
        ]
        return recent_transactions
    
    def apply_interest(self, rate: float) -> bool:
        """Apply interest to the account balance"""
        if rate <= 0:
            print("Interest rate must be positive.")
            return False
        
        interest = self.balance * (rate / 100)
        self.balance += interest
        self._add_transaction("Interest applied", interest)
        print(f"Interest of ${interest:.2f} applied. New balance: ${self.balance:.2f}")
        return True
    
    def close_account(self) -> float:
        """Close the account and return final balance"""
        self.is_active = False
        self._add_transaction("Account closed", 0)
        print(f"Account {self.account_number} closed. Final balance: ${self.balance:.2f}")
        return self.balance
    
    def __str__(self) -> str:
        status = "Active" if self.is_active else "Inactive"
        return (f"Account {self.account_number} ({self.account_holder}): "
                f"Balance: ${self.balance:.2f}, Status: {status}")


class SavingsAccount(BankAccount):
    """A specialized bank account for savings with interest features"""
    
    def __init__(self, account_holder: str, initial_balance: float = 0.0, 
                 interest_rate: float = 1.5):
        super().__init__(account_holder, initial_balance)
        self.interest_rate = interest_rate
        self.minimum_balance = 50.0  # Minimum balance requirement
    
    def apply_monthly_interest(self):
        """Apply monthly interest to the savings account"""
        interest = self.balance * (self.interest_rate / 100) / 12
        self.balance += interest
        self._add_transaction("Monthly interest", interest)
        print(f"Monthly interest of ${interest:.2f} applied.")
    
    def withdraw(self, amount: float) -> bool:
        """Override withdraw to enforce minimum balance"""
        if self.balance - amount < self.minimum_balance:
            print(f"Cannot withdraw. Minimum balance of ${self.minimum_balance:.2f} must be maintained.")
            return False
        return super().withdraw(amount)


def create_sample_accounts() -> List[BankAccount]:
    """Create sample accounts for testing"""
    accounts = [
        BankAccount("John Doe", 1000.0),
        BankAccount("Jane Smith", 2500.0),
        SavingsAccount("Bob Johnson", 500.0, 2.0),
        BankAccount("Alice Brown", 750.0)
    ]
    return accounts


def demonstrate_account_operations():
    """Demonstrate various account operations"""
    print("=== Bank Account System Demonstration ===\n")
    
    # Create sample accounts
    accounts = create_sample_accounts()
    account1, account2, savings_account, account3 = accounts
    
    # Display initial account information
    for account in accounts:
        print(account)
    print()
    
    # Perform various operations
    account1.deposit(500.0)
    account2.withdraw(300.0)
    account1.transfer(200.0, account2)
    savings_account.apply_interest(savings_account.interest_rate)
    
    # Try to withdraw more than minimum balance from savings
    savings_account.withdraw(470.0)
    
    # Show recent transactions for one account
    print(f"\nRecent transactions for {account1.account_holder}:")
    transactions = account1.get_statement(7)
    for t in transactions:
        print(f"{t['date'].strftime('%Y-%m-%d')}: {t['description']} - "
              f"${t['amount']:.2f} (Balance: ${t['balance_after']:.2f})")
    
    # Close an account
    closed_balance = account3.close_account()
    print(f"\nClosed account balance returned: ${closed_balance:.2f}")


if __name__ == "__main__":
    demonstrate_account_operations()