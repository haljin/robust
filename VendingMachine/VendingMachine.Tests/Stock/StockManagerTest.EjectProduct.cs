using System;
using System.Collections.Generic;
using System.Xml.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;

namespace VendingMachine.Stock
{

    public partial class StockManagerTest
    {
        
        // Scenario: Correct product ejected
        // Expected outcome: Product ejected
        [TestMethod]
        public void EjectProductCorrect()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            LinkedList<Product> list = new LinkedList<Product>();


            stockMan.EjectProduct(new Product("faxe"),list);

        }


        // Scenario: Not existing  products ejected
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void EjectProductNonExistent()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            LinkedList<Product> list = new LinkedList<Product>();

            try
            {
                stockMan.EjectProduct(new Product("lol"), list);
                Assert.Fail();
            }
            catch (Exception)
            { }
           

        }

        // Scenario: List is null
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void EjectProductListNull()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
           

            try
            {
                stockMan.EjectProduct(new Product("daim"), null);
                Assert.Fail();
            }
            catch (Exception)
            { }


        }

        // Scenario: Product is null
        // Expected outcome: Exception should be thrown
        [TestMethod]
        public void EjectProductProductNull()
        {
            string doc =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>10</Price>
                <Ammount>5</Ammount>    
              </Product>
              <Product>
                <Name>faxe</Name>
                <Price>12</Price>
                <Ammount>10</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";
            StockManager stockMan = new StockManager(XDocument.Parse(doc));
            LinkedList<Product> list = new LinkedList<Product>();


            try
            {
                stockMan.EjectProduct(new Product(null), list);
                Assert.Fail();
            }
            catch (Exception)
            { }


        }
    }
}
