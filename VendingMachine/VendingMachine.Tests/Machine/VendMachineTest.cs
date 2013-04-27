using System;

using System.Collections.Generic;
using System.Xml.Linq;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.Data;
using VendingMachine.Machine;

namespace VendingMachine.Machine
{
    /// <summary>This class contains parameterized unit tests for VendMachine</summary>
    [PexClass(typeof(VendMachine))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public class VendMachineTest
    {

        // Scenario: Product has been requested, correct ammount of money input
        // Expected outcome: Product should be ejected, change=0
        [TestMethod]
        public void VendMachineCorrect() 
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
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

            VendMachine cos = new VendMachine(XDocument.Parse(doc1),XDocument.Parse(doc));
            TransactionResult result=  cos.SelectProduct(new Product("daim"));
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.InsertCoin(Coin.Kr5);
            Assert.AreNotEqual(TransactionResult.Failure, result);
            result = cos.InsertCoin(Coin.Kr2);
            Assert.AreNotEqual(TransactionResult.Failure, result);
            result= cos.Finalize();
            Assert.AreEqual(TransactionResult.Success, result);
            
        }
        // Scenario: Product has been requested, not enough money inputed
        // Expected outcome: Product should not be ejected
        [TestMethod]
        public void VendMachineNotEnoughInMoney()
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
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

            VendMachine cos = new VendMachine(XDocument.Parse(doc1), XDocument.Parse(doc));
            TransactionResult result = cos.SelectProduct(new Product("daim"));
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.InsertCoin(Coin.Kr5);
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.Finalize();
            Assert.AreEqual(TransactionResult.Failure, result);

        }
        // Scenario: Product has been requested, correct ammount of money inputed,machine does not have change
        // Expected outcome: Product should be ejected
        [TestMethod]
        public void VendMachineNoChange()
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
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

            VendMachine cos = new VendMachine(XDocument.Parse(doc1), XDocument.Parse(doc));
            TransactionResult result = cos.SelectProduct(new Product("daim"));
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.InsertCoin(Coin.Kr5);
            Assert.AreNotEqual(TransactionResult.Failure, result);
            result = cos.InsertCoin(Coin.Kr5);
            Assert.AreEqual(TransactionResult.SuccessButNoChange, result);
            result = cos.Finalize();
            Assert.AreEqual(TransactionResult.Success, result);

        }


        // Scenario: Product has been requested, correct ammount of money inputed, product out of stock
        // Expected outcome: Product should not be ejected
        [TestMethod]
        public void VendMachineOutOfStock()
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
            @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
                <Product>
                <Name>coke</Name>
                <Price>5</Price>
                <Ammount>0</Ammount>    
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

            VendMachine cos = new VendMachine(XDocument.Parse(doc1), XDocument.Parse(doc));
            TransactionResult result = cos.SelectProduct(new Product("coke"));
            Assert.AreEqual(TransactionResult.OutOfStock, result);
            try
            {
                result = cos.InsertCoin(Coin.Kr5);
                result = cos.Finalize();
                Assert.Fail();
            }
            catch (Exception) { }
            

        }

        // Scenario: Cancel test
        // Expected outcome: Should be canceled
        [TestMethod]
        public void VendMachineCancel()
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
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

            VendMachine cos = new VendMachine(XDocument.Parse(doc1), XDocument.Parse(doc));
            TransactionResult result = cos.SelectProduct(new Product("daim"));
            Assert.AreNotEqual(TransactionResult.Failure, result);

            cos.Cancel();
            result = cos.SelectProduct(new Product("faxe"));
            Assert.AreNotEqual(TransactionResult.Failure, result);
            cos.Cancel();
             result = cos.SelectProduct(new Product("daim"));
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.InsertCoin(Coin.Kr5);
            Assert.AreNotEqual(TransactionResult.Failure, result); 
            cos.Cancel();
            result = cos.SelectProduct(new Product("faxe"));
            Assert.AreNotEqual(TransactionResult.Failure, result);
            result = cos.InsertCoin(Coin.Kr2);
            Assert.AreNotEqual(TransactionResult.Failure, result);
            result = cos.InsertCoin(Coin.Kr20);
            Assert.AreNotEqual(TransactionResult.Failure, result);

            result = cos.Finalize();
            Assert.AreEqual(TransactionResult.Success, result);

        }

        // Scenario: Checking product avaiability
        // Expected outcome: Product available/not avaiable
        [TestMethod]
        public void VendMachineProductAvailable()
        {
            string doc =
           @"<?xml version=""1.0"" encoding=""utf-8"" ?>
            <Root>
              <Coin>
                <Type>0.5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>1</Type>
                <Ammount>1</Ammount>
              </Coin>
              <Coin>
                <Type>2</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>5</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>10</Type>
                <Ammount>0</Ammount>
              </Coin>
              <Coin>
                <Type>20</Type>
                <Ammount>0</Ammount>
              </Coin>  
              </Root>";
            string doc1 =
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
                <Ammount>0</Ammount>
              </Product>
              <Product>
                <Name>daim</Name>
                <Price>7</Price>
                <Ammount>1</Ammount>
              </Product>
              </Root>";

            VendMachine cos = new VendMachine(XDocument.Parse(doc1), XDocument.Parse(doc));
            TransactionResult result = cos.CheckProduct(new Product("daim"));
            
            Assert.AreEqual(TransactionResult.Success, result);
            result = cos.CheckProduct(new Product("faxe"));

            Assert.AreEqual(TransactionResult.OutOfStock, result);

        }
    }
}
