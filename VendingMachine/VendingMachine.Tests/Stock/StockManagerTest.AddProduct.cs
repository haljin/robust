using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Stock;
using VendingMachine.Data;
using System.Xml;
using System.Xml.Linq;

namespace VendingMachine.Stock
{

    public partial class StockManagerTest
    {


        // Scenario: Correct product added 
        // Expected outcome: Product added
        [TestMethod]
        public void AddProductCorrect()
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

            stockMan.AddProduct(new Product ("faxe",12,12));

        }


        // Scenario: add really! big number of products 
        // Expected outcome: Product added
        [TestMethod]
        public void AddProductMaxValInt()
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

            stockMan.AddProduct(new Product("faxe", Int32.MaxValue, 12));

        }

        // Scenario: Incorrect product parameter 
        // Expected outcome: Exception thrown (precondition failed)
        [TestMethod]
        public void AddProductIncorrectAmountParameter()
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
                stockMan.AddProduct(new Product("faxe", -9, 12));
                Assert.Fail();
            }
            catch (Exception)
            { }
            

        }

        // Scenario: Adding non-existant product 
        // Expected outcome: Product added
        [TestMethod]
        public void AddProductNonExististantProduct()
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

 
            stockMan.AddProduct(new Product("lol",1, 12));
    


        }
        // Scenario: Product is null
        // Expected outcome: Exception thrown (precondition failed)
        [TestMethod]
        public void AddProductNull()
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
                stockMan.AddProduct(null);
                Assert.Fail();
            }
            catch (Exception)
            { }


        }
        // Scenario: Adding product with different price 
        // Expected outcome: Price change for a product
        [TestMethod]
        public void AddProductDifferentPrice()
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

         
                stockMan.AddProduct(new Product("faxe", 9, 1));

                

        }

        // Scenario: Adding product with an incorrect price parameter 
        // Expected outcome: Excepcion should be thrown
        [TestMethod]
        public void AddProductPriceWrongParam()
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
                stockMan.AddProduct(new Product("faxe", 9, -10));

                Assert.Fail();
            }
            catch (Exception)
            { }





        }
    }
}
