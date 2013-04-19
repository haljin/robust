// <copyright file="Form1Test.cs">Copyright ©  2013</copyright>
using System;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.Validation;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VendingMachine.GUI;

namespace VendingMachine.GUI
{
    /// <summary>This class contains parameterized unit tests for Form1</summary>
    [PexClass(typeof(Form1))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(InvalidOperationException))]
    [PexAllowedExceptionFromTypeUnderTest(typeof(ArgumentException), AcceptExceptionSubtypes = true)]
    [TestClass]
    public partial class Form1Test
    {
        /// <summary>Test stub for .ctor()</summary>
        [PexMethod]
        public Form1 Constructor()
        {
            Form1 target = new Form1();
            return target;
            // TODO: add assertions to method Form1Test.Constructor()
        }
    }
}
