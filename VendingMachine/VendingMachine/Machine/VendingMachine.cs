using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
using VendingMachine.Data;
using VendingMachine.Stock;

namespace VendingMachine.Machine
{

    public enum TransactionResult
    {
        Success, SuccessButNoChange, OutOfStock, Failure
    }

    public class VendMachine
    {
        public Product SelectedProduct { get; set; }
        public decimal InsertedValue { get; set; }
        LinkedList<Coin> CoinCase { get; set; }
        LinkedList<Product> ProductCase { get; set; }
        StockManager StockMan { get; set; }
        CoinManager CoinMan { get; set; }

        public VendMachine()
        {
            StockMan = new StockManager();
            CoinMan = new CoinManager();
            SelectedProduct = null;
            InsertedValue = 0;
            CoinCase = new LinkedList<Coin>();
            ProductCase = new LinkedList<Product>();
        }

        public TransactionResult SelectProduct(Product product)
        {
            if(StockMan.CheckAvailability(product))
            {
                SelectedProduct = product;
                SelectedProduct.Price = StockMan.GetPrice(SelectedProduct);
                if (!CoinMan.CheckChange(SelectedProduct.Price, InsertedValue))
                    return TransactionResult.SuccessButNoChange;
                return TransactionResult.Success;
            }

            return TransactionResult.OutOfStock;

        }

        public TransactionResult CheckProduct(Product product)
        {
            if (StockMan.CheckAvailability(product))
            {
                return TransactionResult.Success;
            }
            return TransactionResult.OutOfStock;

        }

        public TransactionResult InsertCoin(Coin coin)
        {
            InsertedValue += coin.ToValue();

            CoinMan.AddCoin(coin, 1);
            if (SelectedProduct != null && !CoinMan.CheckChange(SelectedProduct.Price, InsertedValue))
                return TransactionResult.SuccessButNoChange;

            return TransactionResult.Success;

        }

        public  void Cancel()
        {
            Contract.Requires(CoinCase != null);
            Contract.Ensures(InsertedValue == 0);
            Contract.Ensures(Contract.OldValue<decimal>(InsertedValue) == CoinCase.Aggregate<Coin, decimal>(0, (acc, c) => acc + c.ToValue()),
                "Ensure the same value is returned");

            CoinMan.GiveChange(0, InsertedValue, CoinCase);
            InsertedValue = 0;
            SelectedProduct = null;            
        }

        public TransactionResult Finalize()
        {
            Contract.Requires(SelectedProduct != null);
            if (SelectedProduct.Price <= InsertedValue)
            {
                PerformTransaction();
                SelectedProduct = null;
                return TransactionResult.Success;
            }

            return TransactionResult.Failure;
        }

        void PerformTransaction()
        {
            Contract.Requires(SelectedProduct.Price <= InsertedValue);
            Contract.Ensures(ProductCase.Contains(SelectedProduct));

            StockMan.EjectProduct(SelectedProduct, ProductCase);
            CoinMan.GiveChange(SelectedProduct.Price, InsertedValue, CoinCase);
            InsertedValue = 0;
        }

        public String ReleasedProducts()
        {
            String ret = "";
            foreach (Product p in ProductCase)
            {
                ret += "Name: " + p.Name + " Amount: " + p.Ammount + "\n";
            }
            return ret;
        }

        public String ReleasedCoins()
        {
            String ret = "";
            foreach (Coin c in CoinCase)
            {
                ret += "Coin: " + c.ToString() + "\n";
            }
            return ret;
        }

        public void EmptyCases()
        {
            CoinCase.Clear();
            ProductCase.Clear();
        }
    }
}
