using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics.Contracts;
using System.Text;

namespace VendingMachine.Data
{
    class Product
    {
        String Name { get; set; }
        int Ammount { get; set; }
        decimal Price { get; set; }
        
        public Product(String name, int amm, decimal price)
        {
            Name = name;
            Ammount = amm;
            Price = price;
        }

        [ContractInvariantMethod]
        private void ObjectInvariant()
        {
            Contract.Invariant(Ammount >= 0);
            Contract.Invariant(Price >= 0);
        }

    }


}
