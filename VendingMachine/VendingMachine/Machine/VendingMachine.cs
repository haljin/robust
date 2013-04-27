using System;
using System.Collections.Generic;
using System.Diagnostics.Contracts;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Linq;
using VendingMachine.Data;
using VendingMachine.Stock;

namespace VendingMachine.Machine
{

    public enum TransactionResult
    {
        Success, SuccessButNoChange, OutOfStock, Failure
    }

    public enum State
    {
        Idle, ProductChosen, MoneyInserted
    }

    public class VendMachine
    {
        public Product SelectedProduct { get; set; }
        public decimal InsertedValue { get; set; }
        LinkedList<Coin> CoinCase { get; set; }
        LinkedList<Product> ProductCase { get; set; }
        StockManager StockMan { get; set; }
        CoinManager CoinMan { get; set; }
        State state { get; set; }

        public VendMachine()
        {
            state = State.Idle;
            StockMan = new StockManager();
            CoinMan = new CoinManager();
            SelectedProduct = null;
            InsertedValue = 0;
            CoinCase = new LinkedList<Coin>();
            ProductCase = new LinkedList<Product>();
        }

        public VendMachine(XDocument stockDoc, XDocument coinDoc)
        {
            state = State.Idle;
            StockMan = new StockManager(stockDoc);
            CoinMan = new CoinManager(coinDoc);
            SelectedProduct = null;
            InsertedValue = 0;
            CoinCase = new LinkedList<Coin>();
            ProductCase = new LinkedList<Product>();
        }


        public TransactionResult SelectProduct(Product product)
        {
            Contract.Requires(state == State.Idle || state == State.ProductChosen);

            if(StockMan.CheckAvailability(product))
            {
                SelectedProduct = product;
                SelectedProduct.Price = StockMan.GetPrice(SelectedProduct);
                state = State.ProductChosen;
                
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
            Contract.Requires(state == State.ProductChosen || state == State.MoneyInserted);
            Contract.Ensures(state == State.MoneyInserted);


            InsertedValue += coin.ToValue();

            CoinMan.AddCoin(coin, 1);
            state = State.MoneyInserted;
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
            Contract.Ensures(state == State.Idle);

            CoinMan.GiveChange(0, InsertedValue, CoinCase);
            InsertedValue = 0;
            SelectedProduct = null;            
            state = State.Idle;
        }

        public TransactionResult Finalize()
        {
            Contract.Requires(SelectedProduct != null);
            Contract.Requires(state == State.MoneyInserted);


            if (SelectedProduct.Price <= InsertedValue)
            {
                PerformTransaction();
                SelectedProduct = null;
                state = State.Idle;
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
