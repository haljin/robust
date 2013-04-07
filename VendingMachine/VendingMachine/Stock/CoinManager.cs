﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Diagnostics.Contracts;
using System.Xml.Linq;
using VendingMachine.Data;

namespace VendingMachine.Stock
{
    class CoinManager
    {
        XDocument Database { get; set; }
        IEnumerable<XElement> Wallet { get; set; }

        public CoinManager()
        {
            Contract.Requires(File.Exists("CoinDatabase.xml"), "PRE: Database file must exist");
            Contract.Ensures(Wallet != null && Wallet.Count() > 0, "POST: Wallet must contain coin types");

            Database = XDocument.Load("CoinDatabase.xml");
            Wallet = Database.Descendants("Coin");
        }


        public bool CheckChange(decimal price, decimal inserted)
        {
            //Contract.Requires(inserted >= price, "PRE: User must have insterted more money than the price of the product");
            Contract.Requires(Wallet != null);
            if (inserted <= price)
                return true;

            decimal change = inserted - price;

            Coin currCoin = Coin.Kr20;
            while (change > 0 && currCoin >= Coin.Ore50)
            {
                decimal value = currCoin.ToValue();
                int coinAmmount = (int)Wallet.Where(c => (decimal)c.Element("Type") == value).Select(c => c.Element("Ammount")).Single();

                int needed = (int)(change / value);
                if (needed > coinAmmount)
                    change -= coinAmmount * value;
                else
                    change -= needed * value;
                currCoin--;

            }
            if (change == 0)
                return true;

            return false;
        }

        public void GiveChange(decimal price, decimal inserted, LinkedList<Coin> coinCase)
        {
            Contract.Requires(inserted >= price, "PRE: User must have insterted more money than the price of the product");
            Contract.Requires(price >= 0 && inserted >= 0);
            Contract.Requires(Wallet != null);
            Contract.Requires(coinCase != null);

            Contract.Ensures(coinCase.Aggregate<Coin, decimal>(0, (acc, coin) => acc + coin.ToValue()) <= (inserted - price), 
                "POST: The value of ejected coins must be equal to required change or lower (if no coins available)");

            decimal change = inserted - price;

            Coin currCoin = Coin.Kr20;
            while (change > 0 && currCoin >= Coin.Ore50)
            {
                decimal value = currCoin.ToValue();
                int coinAmmount = (int)Wallet.Where(c => (decimal)c.Element("Type") == value).Select(c => c.Element("Ammount")).Single();

                int needed = (int)(change / value);
                if (needed > coinAmmount)
                {
                    change -= coinAmmount * value;
                    EjectCoin(currCoin, coinAmmount, coinCase);
                }
                else
                {
                    change -= needed * value;
                    EjectCoin(currCoin, needed, coinCase);
                }
                currCoin--;
            }  
        }

        public void EjectCoin(Coin coin, int ammount, LinkedList<Coin> coinCase)
        {
            Contract.Requires(coinCase != null);
            Contract.Requires(Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Select(c => (int)c.Element("Ammount")).Single() >= ammount,
                    "PRE: There must be enough of coins of given type to eject");
            Contract.Ensures(coinCase.Where(c => c == coin).Count() == ammount,
                    "POST: The ejected coins must be in the case");
            //Contract.Ensures(Contract.OldValue<IEnumerable<XElement>>(Wallet).Where(c => (decimal)c.Element("Type") == coin.ToValue()).Select(c => (int)c.Element("Ammount")).Single()
            //        - ammount  == Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Select(c => (int)c.Element("Ammount")).Single(),
            //        "POST: The ammount of the coins ejected must be deduced");
            
            XElement soughtCoin = Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Single();
            soughtCoin.Element("Ammount").Value = ((int)soughtCoin.Element("Ammount") - ammount).ToString();
            for (int i = 0; i < ammount; ++i)
                coinCase.AddLast(coin);

             Database.Save("CoinDatabase.xml");
        }

        public void AddCoin(Coin coin, int ammount)
        {
            Contract.Requires(Contract.Exists(Wallet, el => (decimal)el.Element("Type") == coin.ToValue()),
                "PRE: The coin type must exist in the wallet");

            XElement soughtCoin = Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Single();
            soughtCoin.Element("Ammount").Value = ((int)soughtCoin.Element("Ammount") + ammount).ToString();

            Database.Save("CoinDatabase.xml");
        }

    }
}
