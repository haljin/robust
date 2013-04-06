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
            Contract.Requires(File.Exists("StockDatabase.xml"));
            Contract.Ensures(Stock != null && Stock.Count() > 0);
            Contract.Ensures(Database != null);


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
            Contract.Requires(Contract.Exists(Stock, p => (string)p.Element("Name") == product.Name));
            Contract.Requires(Stock != null);
            Contract.Requires(product != null);

            return (decimal)Stock.Where(p => (string)p.Element("Name") == product.Name).Select(p => p.Element("Price")).First();
        }

        public void EjectProduct(Product product, LinkedList<Product> productCase)
        {
            Contract.Requires(CheckAvailability(product));
            Contract.Requires(Stock != null);
            Contract.Requires(productCase != null);
            Contract.Requires(product != null);
            Contract.Ensures(productCase.Contains(product));


            XElement node = Stock.Where(el => (string)el.Element("Name") == product.Name).Single();
            node.Element("Ammount").Value = ((int)node.Element("Ammount") - 1).ToString();

            productCase.AddLast(new Product(product.Name,1,product.Price));

            Database.Save("StockDatabase.xml");
        }

        public void AddProduct(Product product)
        {
            Contract.Requires(Stock != null);
            Contract.Requires(product != null);
            Contract.Requires(product.Ammount > 0);
            Contract.Ensures(Contract.Exists(Stock, p => (string)p.Element("Name") == product.Name && (int)p.Element("Price") == product.Price));

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
