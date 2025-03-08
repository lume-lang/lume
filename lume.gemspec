# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name          = 'lume'
  s.version       = '1.0.0'
  s.required_ruby_version = '>= 3.4'

  s.metadata = {
    'rubygems_mfa_required' => 'true'
  }

  s.authors       = ['Max T. Kristiansen']
  s.email         = ['me@maxtrier.dk']
  s.summary       = 'Comfortable and succinct programming language'
  s.description   = 'Type-safe, expressive and to the point.'
  s.homepage      = 'https://github.com/lume-lang/lume'
  s.license       = 'MIT'
  s.files         = ['Rakefile', *Dir.glob('lib/**/*.rb')]
  s.require_paths = ['lib']
end
