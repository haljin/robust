using System;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics.Contracts;
using System.Text;

namespace VendingMachine.Data
{
    public class Product
    {
        public string Name { get; set; }
        public int Ammount { get; set; }
        public decimal Price { get; set; }

        public Product()
        {
        }

        public Product(string name)
        {
            Name = name;
            Ammount = 0;
            Price = 0;
        }

        public Product(string name, int amm, decimal price)
        {
            Contract.Requires(amm >= 0);
            Contract.Requires(price >= 0);

            Name = name;
            Ammount = amm;
            Price = price;
        }

        public override bool Equals(object obj)
        {
            Product prod = obj as Product;
            if (prod != null)
            {
                return prod.Name.Equals(this.Name);
            }
            return false;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        [ContractInvariantMethod]
        private void ObjectInvariant()
        {
            Contract.Invariant(Ammount >= 0);
            Contract.Invariant(Price >= 0);
        }

    }


}
