require 'test/unit'
require_relative 'plates'

class TestPlates < Test::Unit::TestCase
  def test_cs50
    assert_equal(true, is_valid("CS50"))
  end

  def test_cs05
    assert_equal(false, is_valid("CS05"))
  end

  def test_cs50p
    assert_equal(false, is_valid("CS50P"))
  end

  def test_4500
    assert_equal(false, is_valid("4500"))
  end

  def test_c500
    assert_equal(false, is_valid("c500"))
  end

  def test_pi
    assert_equal(false, is_valid("PI3.14"))
  end

  def test_h
    assert_equal(false, is_valid("H"))
  end

  def test_outatime
    assert_equal(false, is_valid("OUTATIME"))
  end
end
