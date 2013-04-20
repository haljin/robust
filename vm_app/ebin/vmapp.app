{application, vmapp,
  [{description,  "The Vending Machine"},
   {vsn,          "1.0"},
   {modules,      [vm_case, vm_coin, vm_coincase, vm_control, vm_display, vm_stock, vm_sup, vmcase_sup, vmdb_sup, vmuser_sup]},
   {registered,   [vm_case, vm_coin, vm_coincase, vm_control, vm_display, vm_stock, vm_sup, vmcase_sup, vmdb_sup, vmuser_sup]},
   {applications, [kernel,stdlib]},
   {mod,          {vmapp,[]}},
   {env, []}
   ]}.
   
