# Day 3 experiments

# Open Classes
class NilClass
    def blank?
        true
    end
end
class String
    def blank?
        self.size == 0
    end
end
["", "person", nil].each do |element|
    puts element unless element.blank?
end

class Numeric
    def inches
        self
    end
    def feet
        self * 12.inches
    end
    def yards
        self * 3.feet
    end
    def miles
        self * 5280.feet
    end
    def back
        self * -1
    end
    def forward
        self
    end
end
puts 10.miles.back
puts 2.feet.forward

# Via method_missing
class Roman
    def self.method_missing name, *args
        roman = name.to_s
        roman.gsub!("IV","IIII")
        roman.gsub!("IX","VIIII")
        roman.gsub!("XL","XXXX")
        roman.gsub!("XC","LXXXX")
        (roman.count("I") +
         roman.count("V") * 5 + 
         roman.count("X") * 10 + 
         roman.count("L") * 50 +
         roman.count("C") * 100)
    end
end
puts
puts Roman.X
puts Roman.XC
puts Roman.XII
puts Roman.X

# Modules

# using methods
class ActsAsCsv
    def read
        file = File.new(self.class.to_s.downcase + '.txt')
        @headers = file.gets.chomp.split(', ')
        file.each do |row|
            @result << row.chomp.split(', ')
        end
    end
    def headers
        @headers
    end
    def csv_contents
        @result
    end
    def initialize
        @result = []
        read
    end
end
class RubyCsv < ActsAsCsv
end
m = RubyCsv.new
puts
puts m.headers.inspect
puts m.csv_contents.inspect

# using module methods aka macros
class ActsAsCsv
    def self.acts_as_csv
        define_method 'read' do
            file = File.new(self.class.to_s.downcase + '.txt')
            @headers = file.gets.chomp.split(', ')
            file.each do |row|
                @result << row.chomp.split(', ')
            end
        end
        define_method "headers" do
            @headers
        end    
        define_method "csv_contents" do
            @result
        end
        define_method 'initialize' do
            @result = []
            read
        end
    end
end

class RubyCsv < ActsAsCsv
end
m = RubyCsv.new
puts
puts m.headers.inspect
puts m.csv_contents.inspect

# using modules
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
                @csv_contents << row.chomp.split(', ')
            end
        end
        attr_accessor :headers, :csv_contents
        def initialize
            read
        end
    end
end
class RubyCsv
    include ActsAsCsvMod
    acts_as_csv
end
m = RubyCsv.new
puts
puts m.headers.inspect
puts m.csv_contents.inspect

# Notes
# Method		
# Can be defined on any object, class, or module		
# Can be called by the object that defines it or its descendants		
# Can be instance methods or class methods		
# Can access instance variables and class variables		

# Module
# Can only be defined with the module keyword
# Cannot be called directly, but can be included or extended by other classes or modules
# Can have both instance methods and module methods
# Can define module variables and constants

# Module Method
# Can only be defined on a module
# Can be called by the module that defines it or any object that extends it
# Are always module methods
# Can access module variables and constants