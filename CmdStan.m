(* ::Package:: *)

(* :Title: MathematicaStan, a MMA interface to CmdStan *)
(* :Context: CmdStan` *)
(* :Author: Vincent Picaud *)
(* :Date: 2019 *)
(* :Package Version: 2.0 *)
(* :Mathematica Version: 11+ *)
(* :Keywords: Stan, CmdStan, Bayesian *)


BeginPackage["CmdStan`"];

Unprotect @@ Names["CmdStan`*"];
ClearAll @@ Names["CmdStan`*"];


(* ::Chapter:: *)
(*Public declarations*)


(* ::Subchapter:: *)
(*Messages*)


CmdStan::cmdStanDirectoryNotDefined="CmdStan directory does not exist, use SetCmdStanDirectory[dir] to define it. This is something like SetCmdStanDirectory[\"~/GitHub/cmdstan/\"]";
CmdStan::usage="Reserved symbol for error messages";
CmdStan::incorrectFileExtension="Expected \".`1`\", got \".`2`\"";
CmdStan::stanExeNotFound="Stan executable \"`1`\" not found.";
CmdStan::stanOutputFileNotFound="Stan output file \"`1`\" not found.";
CmdStan::stanDataFileNotFound="Stan data file \"`1`\" not found.";
CmdStan::OSsupport="MathematicaStan does not support this OS=`1`";
CmdStan::optionSupport="The option \"`1`\" is not supported in this context";


(* ::Subchapter:: *)
(*Options*)


StanVerbose::usage="A boolean option to define verbosity.";
Chains::usage="An integer option to define the number of chains to use in sampling.";
ShowProgress::usage="If True, progress information will be reported during sampling."
PrintStdout::usage="If True, sampler standard output is printed."


(* ::Subchapter:: *)
(*Functions*)


$CmdStanConfigurationFile;
GetCmdStanDirectory;
SetCmdStanDirectory;


ExportStanCode;
CompileStanCode;


StanOptions;

OptimizeDefaultOptions;
SampleDefaultOptions;
VariationalDefaultOptions;

StanOptionExistsQ;
GetStanOption;
SetStanOption;
RemoveStanOption;


ExportStanData;


RunStan;
RunStanSample;


StanResult;
ImportStanResult;


GetStanResult;
GetStanResultMeta;


StanResultKeys;
StanResultMetaKeys;
StanResultReducedKeys;
StanResultReducedMetaKeys;


(* ::Chapter:: *)
(*Private*)


Begin["`Private`"];


(* ::Subchapter:: *)
(*Stan options*)


StanOptions::usage="A structure that holds options";

OptimizeDefaultOptions::usage="Default options for likelihood optimization";
SampleDefaultOptions::usage="Default options for HMC sampling";
VariationalDefaultOptions::usage="Default options for Variational Bayes";

OptimizeDefaultOptions=StanOptions[<|"method"->{"optimize",<||>}|>];
SampleDefaultOptions=StanOptions[<|"method"->{"sample",<||>}|>];
VariationalDefaultOptions=StanOptions[<|"method"->{"variational",<||>}|>];


(* ::Subchapter:: *)
(*Files & Directories*)


$CmdStanConfigurationFile::usage="User configuration file name. Mainly used to store CmdStan directory.";
$CmdStanConfigurationFile=FileNameJoin[{$UserBaseDirectory,"ApplicationData","CmdStanConfigurationFile.txt"}];


GetCmdStanDirectoryQ[]:=FileExistsQ[$CmdStanConfigurationFile]&&DirectoryQ[Import[$CmdStanConfigurationFile]]


GetCmdStanDirectory::usage="GetCmdStanDirectory[] returns CmdStan directory";
GetCmdStanDirectory[]=If[!GetCmdStanDirectoryQ[],Message[CmdStan::cmdStanDirectoryNotDefined];$Failed,Import[$CmdStanConfigurationFile]];


SetCmdStanDirectory::usage="SetCmdStanDirectory[directory_String] modifies CmdStanDirectory";
SetCmdStanDirectory[directory_String]:=(Export[$CmdStanConfigurationFile,directory];directory)


(* ::Subchapter:: *)
(*File extensions helper*)


FileMultipleExtension[fileName_String]:=
	Module[{res},
	       res=StringSplit[FileNameTake@fileName,"."];
	       If[Length[res]==1,
		  res="",
		  res=StringTake[Fold[#1<>"."<>#2&,"",res[[2;;-1]]],2;;-1]
	       ];
	       res
	];


CheckFileNameExtensionQ[fileName_String, expectedExt_String] :=
	Module[{ext,ok},
	       ext = FileMultipleExtension[fileName];
	       ok = ext == expectedExt;
	       If[Not@ok,Message[CmdStan::incorrectFileExtension, expectedExt, ext]];
	       ok];


generateStanExecFileName[stanFileName_String] :=
	Module[{stanExecFileName},
     	       stanExecFileName = FileNameJoin[{DirectoryName[stanFileName], FileBaseName[stanFileName]}];
      	       (* Jeff Patterson Windows fix: originaly with StringReplace[stanExecFileName,"\\"->"/"] TODO*)
      	       If[$OperatingSystem == "Windows",stanExecFileName = stanExecFileName <> ".exe"];  
	       stanExecFileName     	   
	];

generateStanDataFileName[stanFileName_String] :=
	Module[{stanDataFileName},
	       (* caveat: use FixedPoint beacause of .data.R *)
     	       stanDataFileName = FileNameJoin[{DirectoryName[stanFileName], FixedPoint[FileBaseName,stanFileName]}];
     	       stanDataFileName = stanDataFileName <> ".data.R";
      	       stanDataFileName     	   
	];

generateStanOutputFileName[stanFileName_String,processId_Integer?NonNegative] :=
	Module[{stanOutputFileName},
     	       stanOutputFileName = FileNameJoin[{DirectoryName[stanFileName], FileBaseName[stanFileName]}];
     	       If[processId>0,
     	          stanOutputFileName = stanOutputFileName <> "_" <> ToString[processId]
     	       ];
     	       stanOutputFileName = stanOutputFileName <> ".csv";
      	       
	       stanOutputFileName     	   
	];


(* ::Subchapter:: *)
(*Stan code*)


ExportStanCode::usage="ExportStanCode[stanCodeFileName_String, stanCode_String] exports Stan code, return filename WITH path (MMA export generally only returns the file name)";

ExportStanCode[stanCodeFileName_String, stanCode_String]:=
	Module[{oldCode},

	       If[!CheckFileNameExtensionQ[stanCodeFileName,"stan"],Return[$Failed]];

	       (* Note: If file content == stanCode -> do nothing *)
	       If[FileExistsQ[stanCodeFileName],
		  oldCode=Import[stanCodeFileName,"String"],
		  oldCode=""
	       ];
	       If[oldCode!=stanCode,
		  Export[stanCodeFileName,stanCode,"Text"]
	       ];
	       
	       FileNameJoin[{Directory[],stanCodeFileName}]
	];


(* ::Subchapter:: *)
(*Stan code compilation*)


CompileStanCode::usage = "CompileStanCode[stanCodeFileName_String,opts] generates Stan executable (takes some time). Default options {StanVerbose -> True}";

Options[CompileStanCode] = {StanVerbose -> True};

CompileStanCode[stanCodeFileName_String, opts : OptionsPattern[]] :=
	Module[{command, stanExecFileName, verbose, runprocessResult},
	       
	       If[Not@CheckFileNameExtensionQ[stanCodeFileName, "stan"], Return[$Failed]];
	       
	       stanExecFileName = generateStanExecFileName[stanCodeFileName];
	       (* Maybe useful for Windows https://mathematica.stackexchange.com/q/140700/42847 *)
	       command = {"make","-C",GetCmdStanDirectory[],stanExecFileName};
	       
	       verbose = OptionValue[StanVerbose];
	       If[verbose,Print["Running: ",StringRiffle[command," "]]];
	       runprocessResult = RunProcess[command];
	       If[verbose,Print[runprocessResult["StandardOutput"]]];
	       
	       If[runprocessResult["ExitCode"]==0, stanExecFileName, Print[runprocessResult["StandardError"]]; $Failed]   	       
	];


(* ::Section:: *)
(*Convert data to RData, dispatched according to input type*)


(* ::Subsection:: *)
(*Helper that forces scientific notation (use CForm for that)*)


RDumpToStringHelper[V_?VectorQ]:="c("<>StringTake[ToString[Map[CForm,V]],{2,-2}]<>")";


(* ::Subsection:: *)
(*Matrix*)


(* CAVEAT: needs to transpose the matrix to get the right ordering: column major *)
RDumpToString[MatName_String,M_?MatrixQ]:=
	MatName<>" <- structure("<>RDumpToStringHelper[Flatten[Transpose[M]]] <>
	       ", .Dim = "<>RDumpToStringHelper[Dimensions[M]]<>")\n";


(* ::Subsection:: *)
(*Vector*)


RDumpToString[VectName_String,V_?VectorQ]:=VectName<>" <- "<>RDumpToStringHelper[V]<>"\n";


(* ::Subsection:: *)
(*Scalar*)


RDumpToString[VarName_String,Var_?NumberQ]:=VarName<>" <- " <>ToString[Var]<>"\n";


ExportStanData::usage =
"ExportStanData[fileNameDataR_?StringQ,Rdata_Association] creates a .data.R file from an association <|\"variable_name\"->value...|>. value can be a scalar, a vector or a matrix";

ExportStanData[stanFileName_String,Rdata_Association]:=
	Module[{str,stanOutputFileName},
	       (* Add .data.R extension if required *)
	       stanOutputFileName=generateStanDataFileName[stanFileName];
	       (* Open file and save data *)
	       str=OpenWrite[stanOutputFileName];
	       If[str===$Failed,Return[$Failed]];
	       WriteString[str,StringJoin[KeyValueMap[RDumpToString[#,#2]&,Rdata]]];
	       Close[str];
	       stanOutputFileName
	];


(* ::Subchapter:: *)
(*Stan option management (Refactoring: simplify and use StanOption[Association]*)


(* ::Section:: *)
(*Command line string*)


stanOptionToCommandLineString[opt_]:=
	Module[{optList,f,stack={}},
	       f[key_String->{value_,recurse_}]:=key<>"="<>ToString[value]<>" "<>stanOptionToCommandLineString[recurse];
	       f[key_String->{Null,recurse_}]:=key<>" "<>stanOptionToCommandLineString[recurse];
	       f[other_List]:=stanOptionToCommandLineString[other];

	       optList=Normal[opt];
	       Scan[AppendTo[stack,f[#]]&,optList];
	       StringJoin[stack]
	];


Format[StanOptions[opt_Association]] := stanOptionToCommandLineString[opt];


(* ::Section:: *)
(*Split option string*)


splitOptionString[keys_String]:=StringSplit[keys,"."];


(* ::Section:: *)
(*Check option (it it exists)*)


(* ::Text:: *)
(*Needs FoldWhile (see https://mathematica.stackexchange.com/questions/19102/foldwhile-and-foldwhilelist )*)


foldWhile[f_,test_,start_,secargs_List]:=
	Module[{last=start},Fold[If[test[##],last=f[##],Return[last,Fold]]&,start,secargs]];


stanOptionExistsQ[StanOptions[opt_Association],{keys__String}]:=
	Module[{status},foldWhile[Last[#1][#2]&,(status=KeyExistsQ[Last[#1],#2])&,{"",opt},{keys}];status]


StanOptionExistsQ::usage="StanOptionExistsQ[opt_StanOptions,optionString_String] check if the option exists";
stanOptionExistsQ[opt_StanOptions,optionString_String]:=stanOptionExistsQ[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Get option*)


getStanOption[StanOptions[opt_Association],{keys__String}]:=
	Module[{status,extracted},
	       extracted=foldWhile[Last[#1][#2]&,(status=KeyExistsQ[Last[#1],#2])&,{"",opt},{keys}];
	       If[status,extracted,$Failed]
	];
getStanOptionValue[opt_StanOptions,{keys__String}]:=With[{result=getStanOption[opt,{keys}]},If[result===$Failed,result,First[result]]];


GetStanOption::usage="GetStanOption[opt_StanOptions, optionString_String] return Stan option value, $Failed if the option is not defined";

GetStanOption[opt_StanOptions, optionString_String]:=getStanOptionValue[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Set option*)


(* ::Text:: *)
(*Compared to the SO answer the Merge associated function nestedMerge[] merge the assoc[[All, 2]] association, value is set to the last not Null element (this is the role of nestedMergeHelper[])*)


nestedMerge[assoc : {__Association}] := Merge[assoc, nestedMerge];
nestedMergeHelper[arg_List] := With[{cleaned = Select[arg, Not[# === Null] &]}, If[cleaned == {}, Null, Last[cleaned]]];
nestedMerge[assoc : {{_, __Association} ..}] := {nestedMergeHelper[assoc[[All, 1]]], Merge[assoc[[All, 2]], nestedMerge]};


setStanOption[StanOptions[org_Association], {keys__String}, value_] := Module[{tmp},
									      tmp = {org, Fold[ <|#2 -> If[AssociationQ[#], {Null, #}, #]|> &, {value, <||>}, Reverse@{keys}]};
									      StanOptions[nestedMerge[tmp]]
								       ];


SetStanOption::usage="SetStanOption[opt_StanOptions, optionString_String, value_] add or overwrite the given Stan option.";
SetStanOption[opt_StanOptions, optionString_String, value_] := setStanOption[opt,splitOptionString[optionString],value];


(* ::Section:: *)
(*Delete option*)


removeStanOption[StanOptions[org_Association], {oneKey_}]:=StanOptions[KeyDrop[org,oneKey]];

removeStanOption[StanOptions[org_Association], {keys__,last_}]:=
	Module[{buffer,indices},
	       If[StanOptionExistsQ[StanOptions[org], {keys,last}]===False,Return[StanOptions[org]]]; (* nothing to do the key path does not exist *)
	       buffer=org;
	       indices=Riffle[{keys},ConstantArray[2,Length[{keys}]]];
	       KeyDropFrom[buffer[[Apply[Sequence,indices]]],last];
	       buffer=FixedPoint[DeleteCases[# ,{Null,<||>},-1]&,buffer];
	       StanOptions[buffer]
	];


RemoveStanOption::usage="RemoveStanOption[opt_StanOptions, optionString_String] remove the given option.";

RemoveStanOption[opt_StanOptions, optionString_String]:=removeStanOption[opt,splitOptionString[optionString]];


(* ::Section:: *)
(*Some helpers*)


completeStanOptionWithDataFileName[stanFileName_String, stanOption_StanOptions] :=
    	Module[{stanDataFileName},
	       
      	       (* 
      	,* Check if there is a data file name in option, 
      	,* if not, try to create one from scratch 
      	,*)
      	       stanDataFileName = GetStanOption[stanOption,"data.file"];
      	       If[stanDataFileName === $Failed,
		  stanDataFileName = generateStanDataFileName[stanFileName];
               ];
               Assert[CheckFileNameExtensionQ[stanDataFileName, "data.R"]];
	       
               SetStanOption[stanOption,"data.file", stanDataFileName]
        ];

completeStanOptionWithOutputFileName[stanFileName_String, stanOption_StanOptions, processId_?IntegerQ] :=
   	Module[{stanOutputFileName},
	       
               (* 
      	,* Check if there is a output file name in option, 
      	,* if not, try to create one from scratch 
      	,*)
               stanOutputFileName = GetStanOption[stanOption,"output.file"];
               
               If[stanOutputFileName === $Failed,
                  stanOutputFileName = generateStanOutputFileName[stanFileName,processId];
               ];
               Assert[CheckFileNameExtensionQ[stanOutputFileName, "csv"]];
	       
               SetStanOption[stanOption,"output.file", stanOutputFileName]
	];


(* ::Subchapter:: *)
(*Stan Run*)


RunStan::usage="RunStan[stanFileName_String, stanOption_StanOptions, opts : OptionsPattern[]] runs Stan.";

Options[RunStan] = {StanVerbose -> True};

RunStan[stanFileName_String, stanOption_StanOptions, opts : OptionsPattern[]] :=
  	Module[{pathExecFileName, mutableOption, command, verbose, runprocessResult },
   	       (* Generate Executable file name (absolute path) 
   		*)
   	       pathExecFileName = generateStanExecFileName[stanFileName];
   	       If[pathExecFileName === $Failed, Return[$Failed]];
	       
   	       (* Generate Data file name (absolute path) and add it to stanOption list *)
   	       mutableOption = completeStanOptionWithDataFileName[pathExecFileName, stanOption];
   	       If[mutableOption === $Failed, Return[$Failed]];
	       
   	       (* Generat Output file name *)
   	       mutableOption = completeStanOptionWithOutputFileName[stanFileName, mutableOption, 0]; (* 0 means -> only ONE output (sequential) *)
   	       
   	       (* Extract stanOptions and compute! *)
   	       command = {pathExecFileName};
   	       command = Join[command,StringSplit[stanOptionToCommandLineString[mutableOption]," "]];
	       
               verbose = OptionValue[StanVerbose];
               If[verbose, Print["Running: ", StringRiffle[command, " "]]];
               runprocessResult = RunProcess[command];
               If[verbose, Print[runprocessResult["StandardOutput"]]];
      	       
   	       If[runprocessResult["ExitCode"] == 0, GetStanOption[mutableOption,"output.file"], Print[runprocessResult["StandardError"]]; $Failed]   	          
  	];


(* ::Subchapter:: *)
(*Stan Parallel HMC*)


progressLineQ[str_String]:=StringContainsQ[str, NumberString ~~ " / " ~~ NumberString ~~ " [" ~~ ___ ~~ NumberString ~~ "%]" ~~ ___ ~~ "(" ~~ "Warmup" | "Sampling" ~~ ")"]

Options[RunStanSample] = {Chains -> 1, ShowProgress -> False, PrintStdout -> False};

RunStanSample[stanFileName_String, stanOption_StanOptions:SampleDefaultOptions, OptionsPattern[]]:=
    Block[{status=""},
           If[TrueQ[OptionValue[ShowProgress]], 
              Monitor[runStanSample[stanFileName, stanOption, OptionValue[Chains], OptionValue[PrintStdout]], status],
              runStanSample[stanFileName, stanOption, OptionValue[Chains], OptionValue[PrintStdout]]
           ]
    ];


generateFullOptions[stanFileName_String, stanOption_StanOptions, pid_Integer:0]:=
    Module[{pathExecFileName, mutableOption},
              (* Generate Executable file name (absolute path) *)
              pathExecFileName = generateStanExecFileName[stanFileName];
              If[pathExecFileName === $Failed, Return[$Failed]];
           
              (* Generate Data file name (absolute path) and add it to stanOption list *)
              mutableOption = completeStanOptionWithDataFileName[pathExecFileName, stanOption];
              If[mutableOption === $Failed, Return[$Failed]];

              (* Generate Output file name *)
              mutableOption = completeStanOptionWithOutputFileName[stanFileName, mutableOption, pid];
           {pathExecFileName, mutableOption}
    ]

runStanSample[stanFileName_String, stanOption_StanOptions:SampleDefaultOptions, chains_Integer, printStdout_?BooleanQ]:=
    Module[{processArgs, processes, output, i, tmp, chainList, refreshInterval=0.05},
           (* Check that options are for sampling *)
           If[Not@stanOptionExistsQ[stanOption, "method"] || getStanOptionValue[stanOption, {"method"}] =!= "sample",
              Print["Argument \"stanOption\" must have method=sample set."];
              Return[$Failed]
           ];

           If[chains > 1,
              chainList = Table["Chain " <> ToString[i] <> ": ", {i, chains}];
              processArgs = Table[generateFullOptions[stanFileName, stanOption, pid], {pid, chains}],
              processArgs = {generateFullOptions[stanFileName, stanOption, 0]}
           ];

           (* Start sampling processes. *)
           processes = Table[StartProcess[{args[[1]], StringSplit@stanOptionToCommandLineString@args[[2]]}], {args, processArgs}];
           output = Table[{}, chains];
           While[AnyTrue[processes, ProcessStatus[#] == "Running"&],
                 (* Collect output from chains. *)
                 For[i = 1, i <= chains, i = i+1,
                     tmp  = ReadString[processes[[i]], EndOfBuffer];
                     If[tmp =!= EndOfFile,
                        output[[i]] = output[[i]] ~Join~ StringSplit[tmp, "\n"]
                     ];
                 ];
                 (* Update progress. *)
                 If[chains > 1,
                    status = Map[Last@*Select[progressLineQ], output];
                    status = MapThread[StringJoin, {chainList, status}];
                    status = TableForm@status,
                    status = Last@Select[progressLineQ]@output[[1]]
                 ];
                 (* Sleep. *)
                 Pause[refreshInterval];
           ];
           (* Collect rest of the output *)
           For[i = 1, i <= chains, i = i+1,
               tmp  = ReadString[processes[[i]], EndOfBuffer];
               If[tmp =!= EndOfFile,
                  output[[i]] = output[[i]] ~Join~ StringSplit[tmp, "\n"]
               ];
           ];
           (* Check if sampling failed. *)
           If[AnyTrue[processes, ProcessInformation[#]["ExitCode"] =!= 0&],
              status = "Failed.";
              Do[
              Print["Chain " <> ToString[i] <> ":"];
              Print[StringRiffle[output[[i]], "\n"]];
              Print["\n"],
              {i, chains}];
              Return[$Failed]
           ];
           (* Sampling succeeded. Return output file/files. *)
           status = "Done.";
           If[printStdout,
              Do[
              Print["Chain " <> ToString[i] <> ":"];
              Print[StringRiffle[output[[i]], "\n"]];
              Print["\n"],
              {i, chains}]
           ];
           If[chains > 1,
              Table[GetStanOption[args[[2]], "output.file"], {args, processArgs}],
              GetStanOption[processArgs[[1,2]], "output.file"]
           ]
    ];


(* ::Chapter:: *)
(*Import CSV file*)


(* ::Subchapter:: *)
(*Structure*)


StanResult::usage="A structure to store Stan Result";


(* ::Section:: *)
(*Helper for pretty prints of variables*)


makePairNameIndex[varName_String]:=StringSplit[varName,"."] /. {name_String,idx___}:> {name,ToExpression /@ {idx}};


varNameAsString[data_Association]:=
	Module[{tmp},
	       tmp=GroupBy[makePairNameIndex /@ Keys[data],First->Last];
	       tmp=Map[Max,Map[Transpose,tmp],{2}];
	       tmp=Map[First[#]<>" "<>StringRiffle[Last[#],"x"]&,Normal[tmp]];
	       tmp=StringRiffle[tmp,", "];
	       tmp
	];

(*varNameAsString[result_StanResult]:=varNameAsString[First[result]["parameter"]];*)


(* ::Subchapter:: *)
(*Import routine*)


ImportStanResult::usage="ImportStanResult[outputCSV_?StringQ] import csv stan output file and return a StanResult structure.
ImportStanResult[outputCSVList_List import multiple stan output files and return a joint StanResult structure.";

ImportStanResult[outputCSV_?StringQ]:= 
	Module[{data,headerParameter,headerMeta,stringParameter,numericParameter,output},
	       If[!CheckFileNameExtensionQ[outputCSV,"csv"],Return[$Failed]];
	       If[!FileExistsQ[outputCSV],Message[CmdStan::stanOutputFileNotFound,outputCSV];Return[$Failed];];

	       data=Import[outputCSV];
	       data=GroupBy[data,Head[First[#]]&]; (* split string vs numeric *)
	       stringParameter=data[String];
	       data=KeyDrop[data,String];
	       numericParameter=Transpose[First[data[]]];
	       data=GroupBy[stringParameter,StringTake[First[#],{1}]&]; (* split # vs other (header) *)
	       Assert[Length[Keys[data]]==2]; (* # and other *)
	       stringParameter=data["#"]; (* get all strings beginning by # *)
	       data=First[KeyDrop[data,"#"]];(* get other string = one line which is header *)
	       headerMeta=Select[First[data],(StringTake[#,{-1}]=="_")&];
	       headerParameter=Select[First[data],(StringTake[#,{-1}]!="_")&];
	       output=<||>;
		     output["filename"]=outputCSV;
	       output["meta"]=Association[Thread[headerMeta->numericParameter[[1;;Length[headerMeta]]]]];
	       output["parameter"]=Association[Thread[headerParameter->numericParameter[[Length[headerMeta]+1;;-1]]]];
	       output["internal"]=<|"pretty_print_parameter"->varNameAsString[output["parameter"]],
	       "pretty_print_meta"->varNameAsString[output["meta"]],
	       "comments"->StringJoin[Riffle[Map[ToString,stringParameter,{2}],"\n"]]|>;

		   StanResult[output]
	];

ImportStanResult[outputCSVList_List]:=
    Module[{results, output},
           results = Table[ImportStanResult[outputCSV][[1]], {outputCSV, outputCSVList}];
           (* Merge results to a single result object. *)
           output = <||>;
           (* Filenames in a list. *)
           output["filename"] = Table[result["filename"], {result, results}];
           (* Meta and parameter data is merged. *)
           output["meta"] = Merge[Table[result["meta"], {result, results}], Flatten];
           output["parameter"] = Merge[Table[result["parameter"], {result, results}], Flatten];
           (* Internal comments are listed, pretty print values equal to first result. *)
           output["internal"] = <||>;
           output["internal"]["pretty_print_parameter"] = First[results]["internal"]["pretty_print_parameter"];
           output["internal"]["pretty_print_meta"] = First[results]["internal"]["pretty_print_meta"];
           output["internal"]["comments"] = Table[result["internal"]["comments"], {result, results}];
           (* Wrap into StanResult. *)
           StanResult[output]
    ];


Format[StanResult[opt_Association]]:="     file: "<>opt["filename"]<>"\n     meta: "<>opt["internal"]["pretty_print_meta"]<>"\nparameter: "<>opt["internal"]["pretty_print_parameter"];


(* ::Subchapter:: *)
(*Get Result*)


(* ::Section:: *)
(*Helper*)


(* ::Text:: *)
(*Given an association try to find the value from the key*)
(*If the key does not exist try to find an array (in the form of key.X.X...)*)
(*If does not exit $Failed*)


createArray[data_Association,varName_String]:=
	Module[{extracted,index,values,dim,array},
	       extracted=KeySelect[data,First[StringSplit[#,"."]]==varName&];
	       If[extracted==<||>,Print["missing key"];Return[$Failed]];
	       If[Keys[extracted]=={varName},Print["is a scalar and not an array"];Return[$Failed]];
	       index=GroupBy[makePairNameIndex /@ Keys[extracted],First->Last];
	       index=index[varName];
	       values=Values[extracted];
	       dim=Map[Max,Transpose[index]];
	       array=ConstantArray["NA",dim];
	       Scan[(array[[Apply[Sequence,Keys[#]]]]=Values[#])&,Thread[index->values]];
	       array
	];


getStanResult[data_Association,varName_String]:=If[KeyExistsQ[data,varName],data[varName],createArray[data,varName]];


(* ::Section:: *)
(*Public*)


GetStanResult::usage=
"GetStanResult[result_StanResult,parameterName_String] returns the parameter from its name"<>
"\nGetStanResult[(f_Function|f_Symbol),result_StanResult,parameterName_String] returns f[parameter] from its name.";
GetStanResult[result_StanResult,parameterName_String] := getStanResult[First[result]["parameter"],parameterName];
GetStanResult[(f_Function|f_Symbol),result_StanResult,parameterName_String] := Map[f,GetStanResult[result,parameterName],{-2}];


GetStanResultMeta::usage=
"GetStanResultMeta[res_StanResult,metaName_String] return meta data form its name."<>
"\nGetStanResultMeta[(f_Function|f_Symbol),result_StanResult,metaName_String] returns the f[meta] form its name.";
GetStanResultMeta[result_StanResult,metaName_String] := getStanResult[First[result]["meta"],metaName];
GetStanResultMeta[(f_Function|f_Symbol),result_StanResult,metaName_String] := Map[f,GetStanResultMeta[result,metaName],{-2}];


(* ::Subsection:: *)
(*Get keys: for arrays only return the "main" key without indices*)


(* ::Subsubsection:: *)
(*Helper*)


stanResultKeys[result_StanResult,key_String]:=Keys[First[result][key]]


stanResultReducedKeys[result_StanResult,key_String]:=DeleteDuplicates[Map[First[StringSplit[#,"."]]&,stanResultKeys[result,key]]];


(* ::Subsubsection:: *)
(*public*)


StanResultKeys::usage="StanResultKeys[result_StanResult] returns the list of parameter names.";
StanResultKeys[result_StanResult]:=stanResultKeys[result,"parameter"];


StanResultMetaKeys::usage="StanResultMetaKeys[result_StanResult] returns the list of meta parameter names.";
StanResultMetaKeys[result_StanResult]:=stanResultKeys[result,"meta"];


StanResultReducedKeys::usage="StanResultReducedKeys[result_StanResult] returns the list of parameter names. Attention for arrays param.X or param.X.X returns only the prefix \"param\"";
StanResultReducedKeys[result_StanResult]:=stanResultReducedKeys[result,"parameter"];


StanResultReducedMetaKeys::usage="StanResultReducedMetaKeys[result_StanResult] returns the list of meta parameter names. Attention for arrays param.X or param.X.X returns only the prefix \"param\"";
StanResultReducedMetaKeys[result_StanResult]:=stanResultReducedKeys[result,"meta"];


(* ::Subsection:: *)
(*With extra function*)


End[]; (* Private *)


Protect @@ Names["CmdStan`*"];

EndPackage[];
