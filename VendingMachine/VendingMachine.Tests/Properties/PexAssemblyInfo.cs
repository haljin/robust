using Microsoft.Pex.Framework.Suppression;
using System;
using __Auxiliary;
using System.Diagnostics.Contracts.Internal;
// <copyright file="PexAssemblyInfo.cs">Copyright ©  2013</copyright>
// Microsoft.Pex.Framework.Settings
[assembly: global::Microsoft.Pex.Framework.Settings.PexAssemblySettings(TestFramework = "VisualStudioUnitTest")]

// Microsoft.Pex.Framework.Instrumentation
[assembly: global::Microsoft.Pex.Framework.Instrumentation.PexAssemblyUnderTest("VendingMachine")]
[assembly: global::Microsoft.Pex.Framework.Instrumentation.PexInstrumentAssembly("System.Core")]
[assembly: global::Microsoft.Pex.Framework.Instrumentation.PexInstrumentAssembly("System.Drawing")]
[assembly: global::Microsoft.Pex.Framework.Instrumentation.PexInstrumentAssembly("System.Windows.Forms")]
[assembly: global::Microsoft.Pex.Framework.Instrumentation.PexInstrumentAssembly("System.Xml.Linq")]

// Microsoft.Pex.Framework.Creatable
[assembly: global::Microsoft.Pex.Framework.Creatable.PexCreatableFactoryForDelegates]

// Microsoft.Pex.Framework.Validation
[assembly: global::Microsoft.Pex.Framework.Validation.PexAllowedContractRequiresFailureAtTypeUnderTestSurface]
[assembly: global::Microsoft.Pex.Framework.Validation.PexAllowedXmlDocumentedException]

// Microsoft.Pex.Framework.Coverage
[assembly: global::Microsoft.Pex.Framework.Coverage.PexCoverageFilterAssembly(global::Microsoft.Pex.Framework.Coverage.PexCoverageDomain.UserOrTestCode, "System.Core")]
[assembly: global::Microsoft.Pex.Framework.Coverage.PexCoverageFilterAssembly(global::Microsoft.Pex.Framework.Coverage.PexCoverageDomain.UserOrTestCode, "System.Drawing")]
[assembly: global::Microsoft.Pex.Framework.Coverage.PexCoverageFilterAssembly(global::Microsoft.Pex.Framework.Coverage.PexCoverageDomain.UserOrTestCode, "System.Windows.Forms")]
[assembly: global::Microsoft.Pex.Framework.Coverage.PexCoverageFilterAssembly(global::Microsoft.Pex.Framework.Coverage.PexCoverageDomain.UserOrTestCode, "System.Xml.Linq")]

// Microsoft.Pex.Framework.Moles
[assembly: global::Microsoft.Pex.Framework.Moles.PexAssumeContractEnsuresFailureAtBehavedSurface]
[assembly: global::Microsoft.Pex.Framework.Moles.PexChooseAsBehavedCurrentBehavior]

// Microsoft.Pex.Linq
[assembly: global::Microsoft.Pex.Linq.PexLinqPackage]

[assembly: PexSuppressUninstrumentedMethodFromType(typeof(Exception))]
[assembly: PexSuppressUninstrumentedMethodFromType(typeof(ContractHelper))]
[assembly: PexSuppressUninstrumentedMethodFromType(typeof(EventHandler<>))]
