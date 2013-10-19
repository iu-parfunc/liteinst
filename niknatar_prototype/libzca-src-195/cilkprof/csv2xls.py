#!/usr/bin/python

#  Copyright (C) 2012 Intel Corporation
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#  * Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
#  * Neither the name of Intel Corporation nor the names of its
#    contributors may be used to endorse or promote products derived
#    from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
#  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
#  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
#  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.

# csv2xls converts the CSV files created by the Cilkprof tool into worksheets
# in an Excel workbook. In addition to converting the files, the script does
# the following:
# * Adds columns to calculate the Parallelism (Work on Work / Span on Work)
#   and Span Parallelism (Span on Work / Span on Span)
# * Hides columns containing the unmangled caller and callee names
# * Hides column containing the full paths
#
# To use csv2xls, you'll need:
# * Python 2.3 - 2.7
# * A copy of the xlwt package which is used to create Excel workbooks.  A
#   pointer to the package download URL is available at
#   http://pypi.python.org/pypi/xlwt .  To install the package, untar it,
#   cd into the directory you just created, and issue the command
#   "python setup.py install"
#
# Usage:
#  csv2xls bb.csv cc.csv out.xls
#
# The CSV files can be specified in either order.  The output Excel workbook
# file must be the 3rd parameter.

import csv
import xlwt
import sys


class ccColumn:
    """
    Base class for a Caller-Callee data column
    
    m_ws - xlwt Worksheet object we're writing to
    m_column - Worksheet column for this data
    m_data_index - Index into the row of data read from the CSV file
    """

    def __init__(self, ws, column, data_index):
        """Constructor.  Save the data into the instance variables"""
        self.m_ws = ws
        self.m_column = column
        self.m_data_index = data_index

    def write(self, row_num, row_data):
        """
        Write information for a cell specified by the row to the
        workerbook writer.  This method must be overridden by all
        deriving classes!
        """
        print "ccColumn.write should have been overridden"


class ccTextColumn(ccColumn):
    """Class for Caller-Callee text column data"""

    def write(self, row_num, row_data):
        """
        Just write the information to the worksheet - no conversion or
        formating.

        Don't forget that xlwt.Worksheet.write expects rows and columns
        to be 0-based.
        """
        self.m_ws.write(row_num - 1, self.m_column, row_data[self.m_data_index])


class ccIntColumn(ccColumn):
    """
    Class for Caller-Callee integer data column

    m_style - xlwt style to be applied to data in this column.  Integers are
    to be displayed with commas grouping every 3 sets of numbers.  So
    1234 displays as 1,234 .
    """

    def __init__(self, ws, column, data_index):
        """
        Constructor.  Pass parameters to our base class and construct the
        style used for this column.
        """
        ccColumn.__init__(self, ws, column, data_index)
        self.m_style = xlwt.easyxf(num_format_str="#,##0")

    def write(self, row_num, row_data):
        """
        Convert the data to an integer and write it to the cell.

        Don't forget that xlwt.Worksheet.write expects rows and columns
        to be 0-based.
        """
        i = int(row_data[self.m_data_index])
        self.m_ws.write(row_num - 1, self.m_column, i, self.m_style)


class ccIndexColumn(ccColumn):
    """
    Class for Caller-Callee index data column

    m_style - xlwt style to be applied to data in this column.  Integers are
    to be displayed without commas grouping every 3 sets of numbers.  So
    1234 displays as 1234 .
    """

    def __init__(self, ws, column, data_index):
        """
        Constructor.  Pass parameters to our base class and construct the
        style used for this column.
        """
        ccColumn.__init__(self, ws, column, data_index)
#        self.m_style = xlwt.easyxf(num_format_str="0")

    def write(self, row_num, row_data):
        """
        Convert the data to an integer and write it to the cell.

        Don't forget that xlwt.Worksheet.write expects rows and columns
        to be 0-based.
        """
        i = int(row_data[self.m_data_index])
#        self.m_ws.write(row_num - 1, self.m_column, i, self.m_style)
        self.m_ws.write(row_num - 1, self.m_column, i)


class ccParallelismCalculationColumn(ccColumn):
    """
    Class for Caller-Callee Paralelism column data

    m_style - xlwt style to be applied to data in this column.  Parallelism
    is displayed as a floating point number with 3 digits to the right of the
    decimal point, as well as thousands separators. So 1234 displays as
    1,234.000 .
    """

    def __init__(self, ws, column, data_index):
        """
        Constructor.  Pass parameters to our base class and construct the
        style used for this column.
        """
        ccColumn.__init__(self, ws, column, data_index)
        self.m_style = xlwt.easyxf(num_format_str="#,##0.000")

    def name_column(self, c):
        """Convert a column index to an Excel column letter

        Limited to the first 26 columns - good enough for this project"""

        names="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        if (c < 0):
            return "Underflow"
        if (c >= len(names)):
            return "Overflow"
        return names[c]

    def write(self, row_num, row_data):
        """
        Insert a formula for this cell.  Note that even though we're using
        "relative" notation, the cell reference must be correct.

        Don't forget that xlwt.Worksheet.write expects rows and columns
        to be 0-based.
        """
        excel_row = str(row_num)
        work_cell = self.name_column(self.m_column-2) + excel_row
        span_cell = self.name_column(self.m_column-1) + excel_row
        f="if({0}={1},1.0,{0}/{1})".format(work_cell, span_cell)
        self.m_ws.write(row_num - 1, self.m_column, xlwt.Formula(f), self.m_style)

def add_bb_data(wbk, csv_file):
    """
    Read the contents of the Basic Block CSV file and insert it into a
    worksheet named Basic Blocks
    """

    print "Merging Basic Block file \"" + csv_file + "\" into workbook..."

    # Open the CSV for reading
    csv_reader = csv.reader(open(csv_file, "r"))

    # Create a worksheet for the basic block data
    sheet = wbk.add_sheet("Basic Blocks")

    # The first line is the headers.  Make them bold
    header_font = xlwt.Font()
    header_font.bold = True

    header_style = xlwt.XFStyle()
    header_style.font = header_font
    
    # Write the headers to the worksheet.  At the same time, generate
    # the array of ccColumn-derived objects which we'll use to write
    # each of the data cells to the worksheet
    columns = []
    column_headers = []

    headers = csv_reader.next()
    c = 0

    for index in range(len(headers)):
        header_text = headers[index]
        column_headers.append(header_text)
        sheet.write(0, c, header_text, header_style)
        if "Basic Block" == header_text:
            columns.append(ccIndexColumn(sheet, c, index))
        elif "Work" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
        elif "Span" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
            c = c + 1
            column_headers.append("Parallelism")
            sheet.write(0, c, "Parallelism", header_style)
            columns.append(ccParallelismCalculationColumn(sheet, c, index))
        else:
            columns.append(ccTextColumn(sheet, c, index))
        c = c+1

    # Insert the remaining data as numbers.  Don't forget that:
    #  * Row numbers returned from reader.line_num are longs and we need to
    #    convert them to ints
    for row_data in csv_reader:
        for col in columns:
            col.write(int(csv_reader.line_num), row_data)

    # Hide columns we don't want to bother the user with
    # "Function Name" is the raw, mangled function name
    # "File Path" can be really long ane we usually only want the file name
    for index in range(len(column_headers)):
        if "Function Name" == column_headers[index]:
            sheet.col(index).hidden = True
        elif "File Path" == column_headers[index]:
            sheet.col(index).hidden = True

    return

def add_cc_data(wbk, csv_file):
    """
    Read the contents of the Caller-Callee CSV file and insert it into a
    worksheet named Caller-Callee
    """

    print "Merging Caller-Callee file \"" + csv_file + "\" into workbook..."

    # Open the CSV for reading
    csv_reader = csv.reader(open(csv_file, "r"))

    # Create a worksheet for the basic block data
    sheet = wbk.add_sheet("Caller-Callee")

    # The first line is the headers.  Make them bold
    header_font = xlwt.Font()
    header_font.bold = True

    header_style = xlwt.XFStyle()
    header_style.font = header_font

    # Write the headers to the worksheet.  At the same time, generate
    # the array of ccColumn-derived objects which we'll use to write
    # each of the data cells to the worksheet
    columns = []
    column_headers = []

    headers = csv_reader.next()
    c = 0

    for index in range(len(headers)):
        header_text = headers[index]
        column_headers.append(header_text)
        sheet.write(0, c, header_text, header_style)
        if "Call Site" == header_text:
            columns.append(ccIndexColumn(sheet, c, index))
        elif "Work on Work" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
        elif "Span on Work" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
            c = c + 1
            column_headers.append("Parallelism")
            sheet.write(0, c, "Parallelism", header_style)
            columns.append(ccParallelismCalculationColumn(sheet, c, index))
        elif "Work on Span" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
        elif "Span on Span" == header_text:
            columns.append(ccIntColumn(sheet, c, index))
            c = c + 1
            column_headers.append("Span Parallelism")
            sheet.write(0, c, "Span Parallelism", header_style)
            columns.append(ccParallelismCalculationColumn(sheet, c, index))
        else:
            columns.append(ccTextColumn(sheet, c, index))
        c = c+1
    
    # Insert the remaining data as numbers.  Don't forget that:
    #  * Row numbers returned from reader.line_num are longs and we need to
    #    convert them to ints
    for row_data in csv_reader:
        for col in columns:
            col.write(int(csv_reader.line_num), row_data)

    # Hide columns we don't want to bother the user with
    # "Caller" and "Callee" are raw, mangled function names
    # "File Path" can be really long ane we usually only want the file name
    for index in range(len(column_headers)):
        if "Caller" == column_headers[index]:
            sheet.col(index).hidden = True
        elif "Callee" == column_headers[index]:
            sheet.col(index).hidden = True
        elif "File Path" == column_headers[index]:
            sheet.col(index).hidden = True
    return

def is_bb_file(filename):
    """
    Sniff at a file and determine if it's the basic block CSV file.

    We do this by looking at the first string in the header line.  If it
    is "Basic Block" then assume that this is a basic block CSV.
    """
    
    f = open(filename)
    l = f.readline()
    f.close()
    words = l.split(",")
    if '"Basic Block"' == words[0]:
        return True
    else:
        return False
    
def is_cc_file(filename):
    """
    Sniff at a file and determine if it's the caller-callee CSV file.

    We do this by reading the first line and looking at the first column
    header.  If it's "Call Site" then assume it's a caller-callee CSV.
    """
    
    f = open(filename)
    l = f.readline()    # Read the header line
    f.close()
    words = l.split(",")
    if '"Call Site"' == words[0]:
        return True
    else:
        return False

# If we didn't get the right number of arguments, display the command syntax
# and exit
if (len(sys.argv) != 4):
    sys.exit("Usage: csv2xls bb.csv cc.csv out.xls")

# Figure out what we were given.  The 3rd parameter is always the Excel
# workbook we're going to create
bb_file = ""
cc_file = ""
xls_file = sys.argv[3]

# Sniff at the first two files to figure out if they're the basic block or
# caller-callee files
if is_bb_file(sys.argv[1]):
    bb_file = sys.argv[1]
elif is_cc_file(sys.argv[1]):
    cc_file = sys.argv[1]

if is_cc_file(sys.argv[2]):
    cc_file = sys.argv[2]
elif is_bb_file(sys.argv[2]):
    bb_file = sys.argv[2]

# If we didn't find both input files, complain and exit
if ("" == bb_file) & ("" == cc_file):
    sys.exit("Failed to find either basic block or caller-callee CSV file")
    
if "" == bb_file:
    sys.exit("Failed to find basic block CSV file")

if "" == cc_file:
    sys.exit("Failed to find caller-callee CSV file")

# Create an empty workbook
wbk = xlwt.Workbook()
add_cc_data(wbk, cc_file)
add_bb_data(wbk, bb_file)
wbk.save(xls_file)
print "Done"
