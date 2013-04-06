using System;
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
            Contract.Requires(File.Exists("CoinDatabase.xml"));
            Contract.Ensures(Wallet != null && Wallet.Count() > 0);

            Database = XDocument.Load("CoinDatabase.xml");
            Wallet = Database.Descendants("Coin");
        }


        public bool CheckChange(decimal price, decimal inserted)
        {
            Contract.Requires(inserted >= price);
            Contract.Requires(Wallet != null);

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
            Contract.Requires(inserted >= price);
            Contract.Requires(Wallet != null);
            Contract.Requires(coinCase != null);

            Contract.Ensures((coinCase.Aggregate<Coin, decimal>(0, (acc, coin) => acc + coin.ToValue()) == (inserted - price)) ||
                    (Wallet.Aggregate<XElement, int>(0, (acc, elem) => acc + (int)elem.Element("Ammount")) == 0));

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
            Contract.Requires(Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Select(c => (int)c.Element("Ammount")).Single() >= ammount);
            Contract.Ensures(coinCase.Where(c => c == coin).Count() == ammount);
            Contract.Ensures(Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Select(c => (int)c.Element("Ammount")).Single() >= 0);

            XElement soughtCoin = Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Single();
            soughtCoin.Element("Ammount").Value = ((int)soughtCoin.Element("Ammount") - ammount).ToString();
            for (int i = 0; i < ammount; ++i)
                coinCase.AddLast(coin);

             Database.Save("CoinDatabase.xml");
        }

        public void AddCoin(Coin coin, int ammount)
        {
            Contract.Requires(Contract.Exists(Wallet, el => (decimal)el.Element("Type") == coin.ToValue()));

            XElement soughtCoin = Wallet.Where(c => (decimal)c.Element("Type") == coin.ToValue()).Single();
            soughtCoin.Element("Ammount").Value = ((int)soughtCoin.Element("Ammount") + ammount).ToString();

            Database.Save("CoinDatabase.xml");
        }

    }
}
