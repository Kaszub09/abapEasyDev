# abapEasyDev
Easy ABAP Development

## Installation
via https://github.com/abapGit/abapGit

## Modules
Most modules are independent. Some may depend on each other or on https://github.com/Kaszub09/abapEasyALV.

| Module | Description | Info |
| - | - | - |
| Change document | Create change document entry directly from code, without SCD0 and cumbersome function modules. Force tracking changes for data elements not marked as such. | [Examples](src/zed_change_document/zed_change_document_ex/zed_change_doc_simple.prog.abap)
| Coding | Encoding/decoding between different codepages; base64; |  
| Datetime | Date and time functions for calculations and parsing/formatting from/to string. Stopwatch. | |
| Documentation | Display documentation window for various objects. | |
| File explorer | File manipulation on aplication server and on user machine via GUI. File dialogue for file/directory picking. | |
| Images | Easy upload/display of images | |
| Logger | Logging independent of SAP Application Log – (BC-SRV-BAL) | [Examples](src/zed_logger/zed_logger_examples/zed_logs_ex_simple_use.prog.abap) [Screens](docs/logger.md) |
| Msg | Message generation from text and sap message structures | |
| Messages | Send emails / messages to user Business Workplace | |
| PDF | PDF display | |
| Popups | Various popups - ask user for confirmation, to fill fields etc. | |
| Program runner | Run program in background and get result to list | |
| RTTS | RTTS services helpers for struct/table description generation | |
| Sapscript | Class for mass reading of sapscript texts from STXL; Programs/shortcuts for Sapscripts backup/transport/copy | |
| Screens | Reusable screens to use directly from code (with PBO/PAI event handler interface ) without creating them every time | [Examples](src/zed_screens/zed_screens_examples/zed_screens_ex_simple_display.prog.abap)|
| Selection | Wrapper for free selections for easy use | [Examples](src/zed_selection/zed_selection_ex/zed_selection_ex_simple_table.prog.abap) |
| Strings | String manipulation functions | |
| Tables comparision | Compare records from tables after/before change - detects which rows were deleted/inserted/modified/duplicated. Works with index tables | |
| Tables conversion | Convert between internal table and Excel file / CSV file / SALV table display | |
| Textbox | Display textbox | |
| Types | Various commonly used types | |
| XML | Nodes classes for generating XML directly from code | [Examples](src/zed_xml/zcl_ed_xml_root.clas.testclasses.abap) |
