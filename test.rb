class Test
  def self.what
    false
  end

  def test
    def another
      false
    end

    true
  end
end

a = Test.new
a.another
Test.what
