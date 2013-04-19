using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics.Contracts;
using System.Xml.Linq;
using VendingMachine.Data;

namespace VendingMachine.Stock
{
    class StockManager
    {

        public XDocument Database { get; set; }
        public IEnumerable<XElement> Stock { get; set; }

        public StockManager()
        {
            Contract.Requires(File.Exists("StockDatabase.xml"), "PRE: Database file must exist");
            Contract.Ensures(Stock != null && Stock.Count() > 0, "POST: Stock must be created and non-empty");   
            Contract.Ensures(Database != null, "POST: Database must be opened");                     


            Database = XDocument.Load("StockDatabase.xml");
            Stock = Database.Descendants("Product");

        }

        public bool CheckAvailability(Product product)
        {
            Contract.Requires(Stock != null);
            Contract.Requires(product != null);

            return Stock.Any(p => (string)p.Element("Name") == product.Name && (int)p.Element("Ammount") > 0);
        }

        public decimal GetPrice(Product product)
        {
            Contract.Requires(Contract.Exists(Stock, p => (string)p.Element("Name") == product.Name), "PRE: Sought item must be known");
            Contract.Requires(Stock != null);
            Contract.Requires(product != null);

            return (decimal)Stock.Where(p => (string)p.Element("Name") == product.Name).Select(p => p.Element("Price")).First();
        }

        public void EjectProduct(Product product, LinkedList<Product> productCase)
        {
            Contract.Requires(CheckAvailability(product), "PRE: Sought item must be in stock");
            Contract.Requires(Stock != null);
            Contract.Requires(productCase != null);
            Contract.Requires(product != null);
            Contract.Ensures(productCase.Contains(product), "POST: Sought item must be ejected");
            Contract.Ensures(Contract.OldValue((int)Stock.Where(el => (string)el.Element("Name") == product.Name).Single().Element("Ammount")) - 1 ==
                   (int)Stock.Where(el => (string)el.Element("Name") == product.Name).Single().Element("Ammount"));

            XElement node = Stock.Where(el => (string)el.Element("Name") == product.Name).Single();
            node.Element("Ammount").Value = ((int)node.Element("Ammount") - 1).ToString();

            productCase.AddLast(new Product(product.Name,1,product.Price));

            Database.Save("StockDatabase.xml");
        }

        public void AddProduct(Product product)
        {
            Contract.Requires(Stock != null);
            Contract.Requires(product != null);
            Contract.Requires(product.Ammount > 0, "PRE: Added ammount must be >= 0");
            Contract.Ensures(Contract.Exists(Stock, p => (string)p.Element("Name") == product.Name && (int)p.Element("Price") == product.Price), "POST: The added item must be in Stock");
            Contract.Ensures((int)Stock.Where(p => (string)p.Element("Name") == product.Name).Select(c => (int)c.Element("Ammount")).Single() > product.Ammount);
            Contract.Ensures(Contract.OldValue((int)Stock.Where(el => (string)el.Element("Name") == product.Name).Single().Element("Ammount")) + product.Ammount ==
                   (int)Stock.Where(el => (string)el.Element("Name") == product.Name).Single().Element("Ammount"));


            if (CheckAvailability(product))
            {
                XElement node = Stock.Where(el => (string)el.Element("Name") == product.Name).Single();

                node.Element("Ammount").Value = ((int)node.Element("Ammount") + product.Ammount).ToString();
            }
            else
            {
                Stock.Last().AddAfterSelf(new XElement("Product", new XElement("Name", product.Name), new XElement("Price", product.Price), new XElement("Ammount", product.Ammount)));

            }

            Database.Save("StockDatabase.xml");

        }





    }
}
