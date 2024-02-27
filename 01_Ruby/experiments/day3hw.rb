# Day 3 Self-Study (Homework)
# Do:
#   1. Modify the CSV application to support an each method to return a CsvRow object. 
#      Use method_missing on that CsvRow to return the value for the column for a given heading

module ActsAsCsvMod
    def self.included(base)
        base.extend ClassMethods
    end
    module ClassMethods
        def acts_as_csv
            include InstanceMethods
        end
    end
    module InstanceMethods
        def read
            @csv_contents = []
            filename = self.class.to_s.downcase + '.txt'
            file = File.new(filename)
            @headers = file.gets.chomp.split(', ')
            file.each do |row|
                @csv_contents << CsvRow.new(@headers, row.chomp.split(', '))
            end
        end
        attr_accessor :headers, :csv_contents
        def initialize
            read
        end
        def each(&block)
            @csv_contents.each &block
        end
    end
end

class CsvRow
    def initialize(headers, values)
        @data = {}
        headers.zip(values).each do |header, value|
            @data[header] = value
        end
    end
    def method_missing(name, *args)
        @data[name.to_s]
    end
end

class RubyCsv
    include ActsAsCsvMod
    acts_as_csv
end

csv = RubyCsv.new
csv.each {|row| puts row.one}