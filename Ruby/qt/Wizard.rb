require 'Qt'
app = Qt::Application.new(ARGV)

def create_intro_page
  page = Qt::WizardPage.new do |p|
    p.title = "Introduction"
  end

  label = Qt::Label.new("This is a wizard")
  label.word_wrap = true
  layout = Qt::VBoxLayout.new do |v|
    v.add_widget(label)
  end
  page.layout = layout
  page
end

wiz = Qt::Wizard.new do |x|
  x.add_page(create_intro_page())
  x.set_window_title("Test Wizard")
  x.show
end

app.exec
